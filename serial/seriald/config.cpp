#include <stdexcept>
#include <fstream>
#include <lib/fmt.h>
#include "config.h"

bool Config::Device::operator==(const Device& other) const {
	return vars == other.vars && canHardKill == other.canHardKill;
}

Config::Config() {
	auto cuauvSoftware = getenv("CUAUV_SOFTWARE");
	if (!cuauvSoftware) {
		throw std::runtime_error("CUAUV_SOFTWARE not set");
	}

	auto vehicle = getenv("CUAUV_VEHICLE");
	if (!vehicle) {
		throw std::runtime_error("CUAUV_VEHICLE not set");
	}

	readConfig("{}/serial/seriald/conf/{}.toml"_format(cuauvSoftware, vehicle),
			std::unordered_set<std::string>());
}

Config::Config(std::string configPath) {
	readConfig(configPath, std::unordered_set<std::string>());
}

Config::Config(std::string configPath, std::unordered_set<std::string> prevConfigs) {
	readConfig(configPath, prevConfigs);
}

void Config::readConfig(std::string configPath, std::unordered_set<std::string> prevConfigs) {
	try {
		if (prevConfigs.find(configPath) != prevConfigs.end()) {
			throw std::runtime_error("Cannot parse '{}' twice"_format(configPath));
		}
		prevConfigs.emplace(configPath);

		// parse this config file
		std::ifstream ifs(configPath);
		if (!ifs.is_open()) {
			throw std::runtime_error("Could not open");
		}

		auto pr = toml::parse(ifs);
		if (!pr.valid()) {
			throw std::runtime_error("Config syntax error:\n{}"_format(pr.errorReason));
		}
		auto& parseVal = pr.value;

		devices = parseDevices(parseVal);
		ports = parseStringArray(parseVal, "ports");

		// parse child config files
		auto includes = parseStringArray(parseVal, "includes");
		for (auto include : includes) {
			auto childPath = "{}/{}"_format(dirName(configPath), include);
			Config childConfig(childPath);
			devices.insert(childConfig.devices.begin(), childConfig.devices.end());
			ports.insert(childConfig.ports.begin(), childConfig.ports.end());
		}

	} catch (std::runtime_error e) {
		throw(std::runtime_error("Failed to parse '{}':\n{}"_format(configPath, e.what())));
	}
}

std::unordered_map<std::string, Config::Device> Config::parseDevices(const toml::Value& parseVal) {
	auto devicesVal = parseVal.find("devices");
	if (!devicesVal || !devicesVal->is<toml::Table>()) {
		return std::unordered_map<std::string, Config::Device>();
	}

	auto devicesTab = devicesVal->as<toml::Table>();
	std::unordered_map<std::string, Device> tmpDevices;
	for (auto devPair : devicesTab) {
		// parse device
		auto name = devPair.first;
		auto& devTableVal = devPair.second;
		if (!devTableVal.is<toml::Table>()) {
			throw std::runtime_error("Device '{}' is not a TOML table"_format(name));
		}
		auto devTable = devTableVal.as<toml::Table>();

		Device device;
		parseDeviceVars(name, devTable, device);
		parseDeviceOptions(name, devTable, device);
		tmpDevices.emplace(devPair.first, device);
	}

	return tmpDevices;
}


void Config::parseDeviceVars(std::string name, const toml::Table& table, Device& device) {
	auto devVarsIt = table.find("vars");
	if (devVarsIt == table.end()) {
		throw std::runtime_error("Failed to find table 'devices.{}.vars'"_format(name));
	}
	if (!devVarsIt->second.is<toml::Table>()) {
		throw std::runtime_error("'devices.{}.vars' is not a table"_format(name));
	}
	auto devVarsTab = devVarsIt->second.as<toml::Table>();

	for (auto varPair : devVarsTab) {
		if (!varPair.second.is<std::string>()) {
			throw std::runtime_error("{} variable '{}' is not a string"_format(name, varPair.first));
		}
		
		std::string shmPath = varPair.second.as<std::string>();
		auto dotLoc = shmPath.find(".");
		if (dotLoc == std::string::npos) {
			throw std::runtime_error("{} variable '{}' maps to invalid shm path '{}'"_format(
						name, varPair.first, shmPath));
		}

		std::string shmGroup = shmPath.substr(0, dotLoc);
		std::string shmVar = shmPath.substr(dotLoc + 1);
		device.vars.emplace(varPair.first, std::make_pair(shmGroup, shmVar));
	}
}

void Config::parseDeviceOptions(std::string name, const toml::Table& table, Device& device) {
	auto optionsIt = table.find("options");
	if (optionsIt != table.end()) {
		auto optionsVal = optionsIt->second;
		if (!optionsVal.is<toml::Table>()) {
			throw std::runtime_error("'devices.{}.options' is not a table"_format(name));
		}

		auto optionsTable = optionsVal.as<toml::Table>();
		const std::string hkOption = "canHardKill";
		auto canHkIt = optionsTable.find(hkOption);
		if (canHkIt != optionsTable.end()) {
			auto hkVal = canHkIt->second;
			if (hkVal.is<bool>()) {
				device.canHardKill = hkVal.as<bool>();
			} else {
				throw std::runtime_error("'devices.{}.options.{}' is not a bool"_format(name, hkOption));
			}
		}
	}
}
	
std::unordered_set<std::string> Config::parseStringArray(const toml::Value& parseVal, std::string name) {
	auto arrayVal = parseVal.find(name);
	if (!arrayVal || !arrayVal->is<toml::Array>()) {
		return std::unordered_set<std::string>();
	}
	auto array = arrayVal->as<toml::Array>();
	if (array.size() == 0) {
		return std::unordered_set<std::string>();
	}

	// toml guarantees array elems are the same type, so if the first element
	// is a string, then all of them are
	if (!array[0].is<std::string>()) {
		return std::unordered_set<std::string>();
	}

	std::unordered_set<std::string> strSet;
	for (auto elem : array) {
		strSet.emplace(elem.as<std::string>());
	}

	return strSet;
}

std::string Config::dirName(std::string path) {
	for (ssize_t i = path.length() - 1; i >= 0; i--) {
		if (path[i] == '/') {
			return path.substr(0, i);
		}
	}
	return "";
}
