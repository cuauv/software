#pragma once

#include <libshm/c/dynamic.h>
#include "config.h"

class SubStatus {
	public:
		SubStatus(const std::shared_ptr<Config> config);

		// Precondition: name is in seriald config
		void deviceConnected(std::string name, bool connected);

		bool getSoftKill() const;
		void postHardKill(std::string deviceName, bool hardKillValid, bool hardKill);

	private:
		mutable std::mutex m_mutex;

		std::unique_ptr<cuauv::dshm::Group> m_switches;
		cuauv::dshm::Var *m_softKillVar, *m_hardKillVar;
		std::unique_ptr<cuauv::dshm::Group> m_motorDesires;
		std::unique_ptr<cuauv::dshm::Group> m_connectedDevices;

		std::unordered_map<std::string, bool> m_hkDevices;
		size_t m_hkVotes, m_uhkVotes;
		enum class HkType { none, kill, unkill };
		HkType m_lastHk;

		void consensusHardKill(std::string deviceName, bool hardKillValid, bool hardKill);
};

