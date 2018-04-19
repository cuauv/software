#include <libshm/c/dynamic.h>
#include <lib/fmt.h>

#include "log.h"
#include "device_list.h"
#include "device.h"

using namespace cuauv;
using namespace cuauv::serial;

#define log(logLevel, message) \
		LOG_DEV(logLevel, m_name, m_port->portName(), message); \

Device::Device(DeviceList* deviceList, std::string name, Config::Device config, std::shared_ptr<SubStatus> subStatus):
	m_deviceList{deviceList}, m_name{name}, m_config{config}, m_subStatus{subStatus}, m_running{false} {}

void Device::start(Manager* port, std::shared_ptr<DeviceInfo> deviceInfo) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);
	if (m_running) return;

	m_port = port;
	initVarMaps(deviceInfo);

	m_watcher = create_watcher();
	m_shmReadThread = std::thread(&Device::readShm, this);

	if (!m_config.canHardKill) {
		log(Log::warn, "Hardkill disabled");
	}

	m_running = true;
	m_subStatus->deviceConnected(m_name, true);
	log(Log::info, "Started. Reset flags: 0x{:02X}"_format(deviceInfo->resetFlags()));
}

Manager* Device::port() const {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	return m_port;
}

void Device::initVarMaps(std::shared_ptr<DeviceInfo> deviceInfo) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	try {
		std::unordered_set<std::string> mappedVarNames;

		// init read vars
		for (auto& readVar : deviceInfo->readVariables()) {
			mappedVarNames.emplace(readVar.name());

			auto shmInfo = matchWithShmVar(readVar);
			if (!shmInfo.first) {
				log(Log::warn, "Read variable {} has no shm mapping"_format(readVar.name()));
				continue;
			}

			try {
				m_deviceList->claimVarWriter(m_name, shmInfo.second);
			} catch (std::runtime_error e) {
				throw std::runtime_error("Failed to claim shm variable for writing:\n{}"_format(e.what()));
			}

			m_serialToShmVar.emplace(readVar, shmInfo.first);
			m_shmPushGroups.emplace(shmInfo.first->group());
		}

		// init write vars
		for (auto& writeVar : deviceInfo->writeVariables()) {
			mappedVarNames.emplace(writeVar.name());

			auto shmInfo = matchWithShmVar(writeVar);
			if (!shmInfo.first) {
				log(Log::warn, "Write variable {} has no shm mapping"_format(writeVar.name()));
				continue;
			}

			m_shmPullGroups.emplace(shmInfo.first->group());
			m_shmToSerialVar.emplace(shmInfo.first, writeVar);
		}

		// warn of unused var mappings
		for (auto& varMapping : m_config.vars) {
			if (mappedVarNames.find(varMapping.first) == mappedVarNames.end()) {
				log(Log::warn, "Variable {} (mapped to {}.{}) never specified by device"_format(
							varMapping.first,
							varMapping.second.first,
							varMapping.second.second));
			}
		}

	} catch (std::runtime_error e) {
		clearVarMaps();
		throw;
	}
}

std::pair<dshm::Var*, std::string> Device::matchWithShmVar(const Variable& serialVar) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);
	auto confIter = m_config.vars.find(serialVar.name());
	if (confIter == m_config.vars.end()) {
		return {nullptr, ""};
	}
	auto groupName = confIter->second.first;
	auto shmVarName = confIter->second.second;

	auto shmGroupIter = m_shmGroups.find(groupName);
	if (shmGroupIter == m_shmGroups.end()) {
		try {
			m_shmGroups.emplace(groupName, dshm::newGroup(groupName));
		} catch (std::invalid_argument e) {
			throw std::runtime_error("shm group '{}' does not exist"_format(groupName));
		}
	}
	auto shmGroup = m_shmGroups[groupName].get();

	auto shmPath = "{}.{}"_format(groupName, shmVarName);
	dshm::Var* shmVar;
	try {
		shmVar = shmGroup->var(shmVarName);
	} catch (std::invalid_argument e) {
		throw std::runtime_error("shm path '{}' does not exist"_format(shmPath));
	}

	if (!typesEqual(shmVar, serialVar)) {
		throw std::runtime_error("shm variable '{}' and read variable '{}' have different types"_format(
					shmPath, serialVar.name()));
	}

	return {shmVar, shmPath};
}

bool Device::typesEqual(const dshm::Var* shmVar, const Variable& serialVar) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	switch(serialVar.type()) {
		case Variable::Type::UINT8:
		case Variable::Type::INT8:
		case Variable::Type::UINT16:
		case Variable::Type::INT16:
			return shmVar->type() == dshm::Var::Type::INT
				|| shmVar->type() == dshm::Var::Type::BOOL;
			break;
		case Variable::Type::FLOAT:
			return shmVar->type() == dshm::Var::Type::DOUBLE;
			break;
		default:
			return false;
	}
}

void Device::readShm() {
	std::unique_lock<std::recursive_mutex> lock(m_mutex);
	if (!m_running) return;

	auto watcher = m_watcher; // cache watcher to use without lock

	// watch all groups asap
	for (auto group : m_shmPullGroups) {
		group->watch(watcher);
	}

	// perform initial write
	std::unordered_set<dshm::Var*> shmPullVars;
	for (auto group : m_shmPullGroups) {
		auto groupPullVars = group->vars();
		shmPullVars.insert(groupPullVars.begin(), groupPullVars.end());
	}
	m_port->submitWrite(bindWriteVars(shmPullVars));

	lock.unlock();

	while (true) {
		wait_watcher(watcher, false);

		lock.lock();
		if (!m_running) break;

		std::unordered_set<BoundVariable> writeVars;
		for (auto group : m_shmPullGroups) {
			// we assume this inner loop can progress changes per group fast enough
			// so we can capture the other groups' changes before they change again
			auto groupWriteVars = bindWriteVars(group->pull());
			writeVars.insert(groupWriteVars.begin(), groupWriteVars.end());
		}
		m_port->submitWrite(writeVars);

		lock.unlock();
	}

	for (auto group : m_shmPullGroups) {
		group->unwatch(watcher);
	}
}

std::unordered_set<BoundVariable> Device::bindWriteVars(std::unordered_set<cuauv::dshm::Var*> shmVars) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	std::unordered_set<BoundVariable> writeVars;
	for (auto shmVar : shmVars) {
		auto shmToSerialVarIter = m_shmToSerialVar.find(shmVar);
		if (shmToSerialVarIter == m_shmToSerialVar.end()) {
			continue;
		}

    /*if (shmToSerialVarIter->second.name().find("LeLed") != std::string::npos) {
      printf("Hack to ignore LEDS worked!\n");
      continue;
    }*/

		if (shmVar->type() == dshm::Var::Type::INT) {
			auto boundVar = shmToSerialVarIter->second.bind(shmVar->cachedInt());
			writeVars.emplace(boundVar);
		} else if (shmVar->type() == dshm::Var::Type::BOOL) {
			auto boundVar = shmToSerialVarIter->second.bind(shmVar->cachedBool());
			writeVars.emplace(boundVar);
		} else {
			auto boundVar = shmToSerialVarIter->second.bind((float)shmVar->cachedDouble());
			writeVars.emplace(boundVar);
		}
	}

	return writeVars;
}

void Device::onRead(const std::unordered_set<BoundVariable>& vars) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);
	if (!m_running) return;

	for (auto& readVar : vars) {
		auto shmVar = m_serialToShmVar[readVar];
        if (!shmVar) continue; // Ignore unbound device variables

		if (readVar.type() == Variable::Type::FLOAT) {
			shmVar->setCache((double)readVar.getFloat());
		} else if (shmVar->type() == dshm::Var::Type::INT) {
			shmVar->setCache(readVar.getInt());
		} else {
			shmVar->setCache((bool)readVar.getInt());
		}
	}

	for (auto group : m_shmPushGroups) {
		group->push();
	}
}

void Device::onWrite(const std::unordered_set<BoundVariable>&) {
	// we don't really care
}

bool Device::getSoftKill() {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	return m_subStatus->getSoftKill();
}

void Device::postHardKill(bool canHardKill, bool hardKill) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);
	if (!m_running) return;

	if (m_config.canHardKill) {
		m_subStatus->postHardKill(m_name, canHardKill, hardKill);
	}
}

void Device::onDisconnect() {
	std::unique_lock<std::recursive_mutex> lock(m_mutex);
	if (!m_running) return;

	m_running = false;
	m_subStatus->deviceConnected(m_name, false);
	log(m_deviceList->disconnecting() ? Log::info : Log::error, "Disconnected");

	// shut down shm read thread
	broadcast_watcher(m_watcher);
	lock.unlock();
	m_shmReadThread.join(); // no other thread should access m_shmReadThread here
	lock.lock();
	destroy_watcher(m_watcher);

	clearVarMaps();
	m_port = nullptr;
	m_deviceList->onDeviceDisconnect(m_name);
}

void Device::clearVarMaps() {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	m_serialToShmVar.clear();
	m_shmPushGroups.clear();
	m_shmPullGroups.clear();
	m_shmToSerialVar.clear();
	m_shmGroups.clear();
}
