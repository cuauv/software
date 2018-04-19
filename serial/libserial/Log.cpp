#include "Log.h"

namespace cuauv {
namespace serial {

Log::log_func_t Log::s_func = nullptr;

void Log::setLogger(log_func_t func) {
    s_func = func;
}

void Log::log(Level level, std::string component, std::string msg) {
    if (s_func) {
        s_func(level, component, msg);
    }
}

void Log::info(std::string component, std::string msg) {
    log(Level::INFO, component, msg);
}

void Log::warn(std::string component, std::string msg) {
    log(Level::WARN, component, msg);
}

void Log::error(std::string component, std::string msg) {
    log(Level::ERROR, component, msg);
}

}} // end namespace cuauv::serial
