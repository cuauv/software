#pragma once

#include <string>
#include <functional>

namespace cuauv {
namespace serial {

/// @brief a simple logger, which library clients can use to receive events
class Log {
    public:
        /// The level of a logged event
        enum class Level {
            INFO,
            WARN,
            ERROR
        };

        /**
         * A callback type for log events. Called whenever a log event occurs, without
         * any sort of locking or synchronization (ie multiple threads may call this
         * simultaneously). Parameters are the log level, a string identifying the
         * component, and the message
         */
        typedef std::function<void(Level, const std::string&, const std::string&)> log_func_t;

        /**
         * Sets the logging function which will be called for logged events
         * 
         * Prior to calling this, all logged events will be dropped
         *
         * @param func the callback function for logging
         */
        static void setLogger(log_func_t func);

        /**
         * Logs an event
         *
         * @param level the level of the log event
         * @param component a string indicating the relevant component
         * @param msg the log message
         */
        static void log(Level level, std::string component, std::string msg);

        /**
         * Logs an INFO event
         *
         * @param component a string indicating the relevant component
         * @param msg the log message
         */
        static void info(std::string component, std::string msg);

        /**
         * Logs a WARN event
         *
         * @param component a string indicating the relevant component
         * @param msg the log message
         */
        static void warn(std::string component, std::string msg);

        /**
         * Logs an ERROR event
         *
         * @param component a string indicating the relevant component
         * @param msg the log message
         */
        static void error(std::string component, std::string msg);

    private:
        /// The logging function
        static log_func_t s_func;
};

}} // end namespace cuauv::serial
