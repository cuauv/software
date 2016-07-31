#pragma once

#include <string>
#include <iostream>
#include <auvlog/client.h>

#include <lib/fmt.h>

enum class Log { info, warn, error };

#define LOG(logLevel, message) \
	switch(logLevel) { \
		case Log::info: \
			auvlog_log("seriald.info", message); \
			fmt::print_colored(fmt::Color::BLUE, "info"); \
			fmt::print("\t{}\n", message); \
			break; \
		case Log::warn: \
			auvlog_log("seriald.warn", message); \
			fmt::print_colored(fmt::Color::YELLOW, "warn"); \
			fmt::print("\t{}\n", message); \
			break; \
		case Log::error: \
			auvlog_log("seriald.error", message); \
			fmt::print_colored(fmt::Color::RED, "error"); \
			fmt::print("\t{}\n", message); \
			break; \
	}

#define LOG_DEV(logLevel, device, port, message) \
	switch(logLevel) { \
		case Log::info: \
			auvlog_log(fmt::format("seriald.device.{}@{}.info", device, port), message); \
			fmt::print_colored(fmt::Color::BLUE, "info"); \
			fmt::print("\t[{}@{}] {}\n", device, port, message); \
			break; \
		case Log::warn: \
			auvlog_log(fmt::format("seriald.device.{}@{}.warn", device, port), message); \
			fmt::print_colored(fmt::Color::YELLOW, "warn"); \
			fmt::print("\t[{}@{}] {}\n", device, port, message); \
			break; \
		case Log::error: \
			auvlog_log(fmt::format("seriald.device.{}@{}.error", device, port), message); \
			fmt::print_colored(fmt::Color::RED, "error"); \
			fmt::print("\t[{}@{}] {}\n", device, port, message); \
			break; \
	}
