#pragma once

#include <memory>
#include <unordered_set>
#include <unordered_map>

#include <mutex>
#include <thread>

#include "config.h"
#include "sub_status.h"
#include "device.h"

class DeviceList {
	public:
		DeviceList(const std::shared_ptr<Config> config, std::shared_ptr<SubStatus> subStatus);
		~DeviceList();

		void onDeviceDisconnect(std::string deviceName);
		void claimVarWriter(std::string deviceName, std::string shmPath);
		bool disconnecting();

	private:
		std::recursive_mutex m_mutex;

		std::unordered_map<std::string, std::shared_ptr<Device>> m_disabledDevices;
		std::unordered_map<std::string, std::shared_ptr<Device>> m_runningDevices;

		std::unordered_map<std::string, std::shared_ptr<cuauv::serial::Manager>> m_ports;
		std::unordered_map<cuauv::serial::Manager*, std::string> m_portOwners; // port -> device name
		std::unordered_map<std::string, std::string> m_varWriters; // shm path -> device name
		std::thread m_portDeleter;

		bool m_disconnecting;

		std::shared_ptr<cuauv::serial::DeviceCallbacks> onPortConnect(cuauv::serial::Manager* port, std::shared_ptr<cuauv::serial::DeviceInfo> deviceInfo);
		void onPortFailure(cuauv::serial::Manager* port, std::string error);
		void deletePort(std::string name);
};
