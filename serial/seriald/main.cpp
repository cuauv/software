#include <iostream>
#include <csignal>

#include <libshm/c/shm.h>
#include <serial/libserial/Log.h>
#include <lib/fmt.h>

#include "log.h"
#include "config.h"
#include "device_list.h"
#include "sub_status.h"

using namespace cuauv;

static std::mutex mut;
static std::condition_variable condVar;
static volatile bool signaled = false;

void onSignal(int) {
	std::lock_guard<std::mutex> lock(mut);
	signaled = true;
	condVar.notify_all();
}

void onLibSerialLog(serial::Log::Level level, std::string component, std::string msg) {
	switch (level) {
		case serial::Log::Level::INFO:
			LOG_DEV(Log::info, "", component, msg);
			break;
		case serial::Log::Level::WARN:
			LOG_DEV(Log::warn, "", component, msg);
			break;
		case serial::Log::Level::ERROR:
			LOG_DEV(Log::error, "", component, msg);
			break;
	}
}

int main(int argc, char** argv) {
	if (argc > 2) {
		fmt::print(std::cerr, "{} <path/to/config.toml>\n", argv[0]);
		return 1;
	}

	shm_init();
	auvlog_init();
    serial::Log::setLogger(&onLibSerialLog);
	LOG(Log::info, "seriald starting...");

	std::shared_ptr<Config> config;
	try {
		if (argc == 2) {
			config = std::make_shared<Config>(argv[1]);
		} else {
			config = std::make_shared<Config>();
		}
	} catch (std::runtime_error e) {
		LOG(Log::error, "Could not parse config file, shutting down:\n{}"_format(e.what()));
		return 1;
	}
	if (config->devices.size() == 0) {
		LOG(Log::error, "No devices specified, shutting down");
		return 1;
	}
	if (config->ports.size() == 0) {
		LOG(Log::error, "No ports specified, shutting down");
		return 1;
	}

	std::shared_ptr<SubStatus> subStatus;
	try {
		subStatus = std::make_shared<SubStatus>(config);
	} catch (std::runtime_error e) {
		LOG(Log::error, "Cannot control sub status, shutting down:\n{}"_format(e.what()));
		return 1;
	}
	DeviceList devList(config, subStatus);

	std::signal(SIGINT, onSignal);
	std::signal(SIGTERM, onSignal);

	std::unique_lock<std::mutex> lock(mut);
	while (!signaled) {
		condVar.wait(lock);
	}

	LOG(Log::info, "Signaled, shutting down...");
}
