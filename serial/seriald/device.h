#pragma once

#include <unordered_map>
#include <thread>

#include <serial/libserial/Manager.h>
#include <serial/libserial/DeviceCallbacks.h>
#include <libshm/c/dynamic.h>
#include <libshm/c/watcher.h>

#include "config.h"
#include "sub_status.h"

class DeviceList;

class Device : public cuauv::serial::DeviceCallbacks {
	public:
		Device(DeviceList* devices, std::string name, Config::Device config, std::shared_ptr<SubStatus> subStatus);

		void start(cuauv::serial::Manager* port, std::shared_ptr<cuauv::serial::DeviceInfo> deviceInfo);
		cuauv::serial::Manager* port() const;

		// DeviceCallbacks implementation
        void onRead(const std::unordered_set<cuauv::serial::BoundVariable>& vars) override;
        void onWrite(const std::unordered_set<cuauv::serial::BoundVariable>&) override;
		bool getSoftKill() override;
		void postHardKill(bool canHardKill, bool hardKill) override;
		void onDisconnect() override;

	private:
		DeviceList* m_deviceList;
		std::string m_name;
		Config::Device m_config;
		std::shared_ptr<SubStatus> m_subStatus;

		mutable std::recursive_mutex m_mutex;
		std::thread m_shmReadThread;

		bool m_running;
		cuauv::serial::Manager* m_port;
		watcher_t m_watcher;

		std::unordered_map<std::string, std::unique_ptr<cuauv::dshm::Group>> m_shmGroups;
		// reading
		std::unordered_map<cuauv::serial::Variable, cuauv::dshm::Var*> m_serialToShmVar;
		std::unordered_set<cuauv::dshm::Group*> m_shmPushGroups;
		// writing
		std::unordered_set<cuauv::dshm::Group*> m_shmPullGroups;
		std::unordered_map<cuauv::dshm::Var*, cuauv::serial::Variable> m_shmToSerialVar;

		void initVarMaps(std::shared_ptr<cuauv::serial::DeviceInfo> deviceInfo);
		std::pair<cuauv::dshm::Var*, std::string> matchWithShmVar(const cuauv::serial::Variable& serialVar);
		bool typesEqual(const cuauv::dshm::Var* shmVar, const cuauv::serial::Variable& serialVar);
		void clearVarMaps();

		void readShm();
		std::unordered_set<cuauv::serial::BoundVariable> bindWriteVars(std::unordered_set<cuauv::dshm::Var*> shmVars);
};
