#include <lib/fmt.h>
#include "log.h"
#include "sub_status.h"

using namespace cuauv;

SubStatus::SubStatus(const std::shared_ptr<Config> config):
	m_switches{dshm::newGroup("switches")},
	m_softKillVar{m_switches->var("soft_kill")},
	m_hardKillVar{m_switches->var("hard_kill")},
	m_motorDesires{dshm::newGroup("motor_desires")},
	m_connectedDevices{dshm::newGroup("connected_devices")},
	m_hkVotes{0}, m_uhkVotes{0},
	m_lastHk{HkType::none}
{
	std::lock_guard<std::mutex> lock(m_mutex);

	for (auto v : {m_softKillVar, m_hardKillVar}) {
		if (v->type() != dshm::Var::Type::BOOL) {
			throw std::runtime_error("'{}' is not a bool"_format(v->path()));
		}
	}

	for (auto v : m_motorDesires->vars()) {
		if (v->type() != dshm::Var::Type::INT) {
			throw std::runtime_error("'{}' is not an int"_format(v->path()));
		}
	}

	for (auto& dev : config->devices) {
		dshm::Var* v;

		try {
			v = m_connectedDevices->var(dev.first);
		} catch (std::invalid_argument e) {
			throw std::runtime_error("Device '{}' not found in shm group '{}'"_format(
					dev.first, m_connectedDevices->name()));
		}

		if (v->type() != dshm::Var::Type::BOOL) {
			throw std::runtime_error("'{}' is not a bool"_format(v->path()));
		}
	}
}

void SubStatus::deviceConnected(std::string name, bool connected) {
	std::lock_guard<std::mutex> lock(m_mutex);

	m_connectedDevices->var(name)->setShm(connected);
	if (!connected) {
		consensusHardKill(name, false, false);
	}
}

bool SubStatus::getSoftKill() const {
	std::lock_guard<std::mutex> lock(m_mutex);

	return m_softKillVar->shmBool();
}

void SubStatus::postHardKill(std::string deviceName, bool hardKillValid, bool hardKill) {
	std::lock_guard<std::mutex> lock(m_mutex);

	consensusHardKill(deviceName, hardKillValid, hardKill);
}

void SubStatus::consensusHardKill(std::string deviceName, bool hardKillValid, bool hardKill) {
	// only called when under lock

	auto it = m_hkDevices.find(deviceName);
	if (hardKillValid) {
		if (it == m_hkDevices.end()) {
			m_hkDevices.emplace(deviceName, hardKill);
			++(hardKill ? m_hkVotes : m_uhkVotes);

		} else if (it->second != hardKill) {
			m_hkDevices[it->first] = hardKill;
			if (hardKill) {
				--m_uhkVotes;
				++m_hkVotes;
			} else {
				--m_hkVotes;
				++m_uhkVotes;
			}
		}

	} else if (it != m_hkDevices.end()) {
		--(it->second ? m_hkVotes : m_uhkVotes);
		m_hkDevices.erase(it);
	}

	if (m_lastHk != HkType::kill && m_hkVotes > m_hkDevices.size()/2) {
		// hardkill
		LOG(Log::info, "Hardkill");
		m_softKillVar->setCache(true);
		m_hardKillVar->setCache(true);
		m_switches->push();

		for (auto v : m_motorDesires->vars()) {
			v->setCache(0);
		}
		m_motorDesires->push();

		m_lastHk = HkType::kill;

	} else if (m_lastHk != HkType::unkill && m_uhkVotes > m_hkDevices.size()/2) {
		// unhardkill
		LOG(Log::info, "Unhardkill");
		m_hardKillVar->setShm(false);

		m_lastHk = HkType::unkill;
	}
}
