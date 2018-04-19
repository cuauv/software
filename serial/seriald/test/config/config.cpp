#include <cstdlib>
#include <string>
#include <gtest/gtest.h>

#include "../../config.h"

class ConfigTest : public testing::Test {
	public:
		ConfigTest() {
			auto cuauvSof = getenv("CUAUV_SOFTWARE");
			if (cuauvSof == nullptr) {
				throw std::runtime_error("CUAUV_SOFTWARE not set");
			}

			configDir = std::string(cuauvSof) + "serial/seriald/test/config/";
		}

	protected:
		std::string configDir;
};

TEST_F(ConfigTest, ValidConfig) {
	std::unordered_map<std::string, Config::Device> expDevices{
		{"hydrophones", {
			{
				{"phaseX", {"hydrophones_results", "phaseX"}},
				{"phaseY", {"hydrophones_results", "phaseY"}},
				{"phaseZ", {"hydrophones_results", "phaseZ"}},
				{"agEnabled", {"hydrophones_settings", "agEnabled"}},
				{"agHigh", {"hydrophones_settings", "agHigh"}},
			}, true}
		},
			
		{"led", {
			{
				{"port_red", {"led", "port_red"}},
			}, false}
		},
	};

	std::unordered_set<std::string> expPorts{
		"/dev/tty0",
		"/dev/tty1",
		"/dev/tty3",
	};

	Config config(configDir + "config.toml");

	EXPECT_EQ(expDevices["hydrophones"], config.devices["hydrophones"]);
	EXPECT_EQ(expDevices["led"], config.devices["led"]);
	EXPECT_EQ(config.devices.find("random_thing"), config.devices.end());

	EXPECT_EQ(expPorts, config.ports);
}

TEST_F(ConfigTest, FileNotFound) {
	try {
		Config(configDir + "blahblahblah.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("Could not open config file", e.what());
	}
}

TEST_F(ConfigTest, SyntaxError) {
	try {
		Config(configDir + "syntaxError.toml");
		FAIL();
	} catch (std::runtime_error e) {
		std::string what = e.what();
		auto newlinePos = what.find("\n");
		EXPECT_EQ("Config syntax error:", what.substr(0, newlinePos));
	}
}

TEST_F(ConfigTest, MissingSection) {
	try {
		Config(configDir + "missingPorts.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("Failed to find 'ports' array", e.what());
	}

	try {
		Config(configDir + "missingDevices.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("Failed to find 'devices' table", e.what());
	}
}

TEST_F(ConfigTest, InvalidDevice) {
	try {
		Config(configDir + "nonTableDevice.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("Device 'led' is not a TOML table", e.what());
	}

	try {
		Config(configDir + "varsNotFound.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("Failed to find table 'devices.hydrophones.vars'", e.what());
	}

	try {
		Config(configDir + "nonStringVar.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("led variable 'port_blue' is not a string", e.what());
	}

	try {
		Config(configDir + "missingGroup.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("led variable 'port_blue' does not specify a shm group", e.what());
	}
}

TEST_F(ConfigTest, Options) {
	try {
		Config(configDir + "nonTableOptions.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("'devices.hydrophones.options' is not a table", e.what());
	}

	try {
		Config(configDir + "nonBoolHardKill.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("'devices.hydrophones.options.canHardKill' is not a bool", e.what());
	}
}

TEST_F(ConfigTest, NonStringPorts) {
	try {
		Config(configDir + "nonStringPorts.toml");
		FAIL();
	} catch (std::runtime_error e) {
		EXPECT_STREQ("'ports' must be an array of strings", e.what());
	}
}
