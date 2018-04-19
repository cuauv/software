#pragma once

#include <string>
#include <mutex>
#include <unordered_map>
#include <unordered_set>
#include <memory>

#include <lib/toml.h>

class Config {
	public:
		Config();
		Config(std::string configPath);

		typedef std::pair<std::string, std::string> ShmPath; // {group, name}
		struct Device {
			bool operator==(const Device& other) const;

			std::unordered_map<std::string, ShmPath> vars;
			bool canHardKill = true;
		};

		std::unordered_map<std::string, Device> devices;
		std::unordered_set<std::string> ports;

	private:
		Config(std::string configPath, std::unordered_set<std::string> prevConfigs);
		void readConfig(std::string configPath, std::unordered_set<std::string> prevConfigs);

		std::unordered_map<std::string, Device> parseDevices(const toml::Value& parseVal);
		void parseDeviceVars(std::string name, const toml::Table& table, Device& device);
		void parseDeviceOptions(std::string name, const toml::Table& table, Device& device);
		std::unordered_set<std::string> parseStringArray(const toml::Value& parseVal, std::string name);

		std::string dirName(std::string path);
};
