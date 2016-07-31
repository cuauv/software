#include <lib/fmt.h>

#include "log.h"
#include "device_list.h"

using namespace std::placeholders;
using namespace cuauv::serial;

DeviceList::DeviceList(const std::shared_ptr<Config> config, std::shared_ptr<SubStatus> subStatus):
	m_disconnecting{false} {
	// initialization done in constructor instead of initializer list partly
	// to ensure Manager callbacks must obtain the lock, so they don't fire during
	// initialization
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	// init devices in "ready" state
	for (auto& dev : config->devices) {
		m_disabledDevices.emplace(dev.first, std::make_shared<Device>(this, dev.first, dev.second, subStatus));
	}

	// init serial ports
	for (auto portName : config->ports) {
		try {
			m_ports.emplace(portName, std::make_shared<Manager>(
						portName,
						std::bind(&DeviceList::onPortConnect, this, _1, _2),
						std::bind(&DeviceList::onPortFailure, this, _1, _2)));

			m_ports[portName]->start(); // start communicating immediately

		} catch (std::exception e) {
			LOG(Log::error, "Could not connect to port '{}'"_format(portName));
		}
	}

	LOG(Log::info, "Initialized device list");
}

DeviceList::~DeviceList() {
	std::unique_lock<std::recursive_mutex> lock(m_mutex);
	auto ports = m_ports;
	m_disconnecting = true;
	lock.unlock();

	for (auto& port : ports) {
		port.second->disconnect();
	}

	if (m_portDeleter.joinable()) {
		m_portDeleter.join();
	}
}

std::shared_ptr<DeviceCallbacks> DeviceList::onPortConnect(Manager* port, std::shared_ptr<DeviceInfo> deviceInfo) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	auto devIt = m_disabledDevices.find(deviceInfo->name());
	if (devIt == m_disabledDevices.end()) {
		auto runDevIt = m_runningDevices.find(deviceInfo->name());
		if (runDevIt != m_runningDevices.end()) {
			LOG_DEV(Log::error, deviceInfo->name(), port->portName(),
					"Could not start because already active on port '{}'"_format(
						runDevIt->second->port()->portName()));
		} else {
			LOG_DEV(Log::error, deviceInfo->name(), port->portName(), "Could not start because config not specified");
		}

		return nullptr;
	}

	auto devName = devIt->first;
	auto dev = devIt->second;
	try {
		dev->start(port, deviceInfo);
	} catch (std::runtime_error e) {
		LOG_DEV(Log::error, devName, port->portName(), "Could not start:\n{}"_format(e.what()));
		return nullptr;
	}

	m_runningDevices.emplace(devName, dev);
	m_disabledDevices.erase(devIt);
	m_portOwners.emplace(port, devName);
	return dev;
}

void DeviceList::claimVarWriter(std::string deviceName, std::string shmPath) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	auto it = m_varWriters.find(shmPath);
	if (it == m_varWriters.end()) {
		m_varWriters.emplace(shmPath, deviceName);
		return;
	} else if (it->second != deviceName) {
		throw std::runtime_error("shm variable '{}' already claimed by {}"_format(shmPath, it->second));
	}
}

bool DeviceList::disconnecting() {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	return m_disconnecting;
}

void DeviceList::onDeviceDisconnect(std::string deviceName) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	auto devIter = m_runningDevices.find(deviceName);
	m_disabledDevices.emplace(deviceName, devIter->second);
	m_runningDevices.erase(devIter);
}

void DeviceList::onPortFailure(Manager* port, std::string error) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	LOG(Log::error, "Failed to connect to port:\n{}"_format(error));

	// clear device's variable write claims and port ownership
	auto portOwnersIter = m_portOwners.find(port);
	auto writersIter = m_varWriters.begin();
	while (writersIter != m_varWriters.end()) {
		if (writersIter->second == portOwnersIter->second) {
			writersIter = m_varWriters.erase(writersIter);
		} else {
			++writersIter;
		}
	}
	m_portOwners.erase(portOwnersIter);

	// delete port
	for (auto it = m_ports.begin(); it != m_ports.end(); ++it) {
		if (it->second.get() == port) {
			if (m_portDeleter.joinable()) {
				m_portDeleter.join();
			}
			m_portDeleter = std::thread(&DeviceList::deletePort, this, it->first);

			break;
		}
	}
}

void DeviceList::deletePort(std::string name) {
	std::lock_guard<std::recursive_mutex> lock(m_mutex);

	auto it = m_ports.find(name);
	if (it != m_ports.end()) {
		m_ports.erase(it);
	}
}
