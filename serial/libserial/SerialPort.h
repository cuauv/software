#pragma once
#include "UnixFilePort.h"

#include <string>
#include <set>

namespace cuauv {
namespace serial {

/**
 * @brief A Port that maps to a physical serial port
 */
class SerialPort : public UnixFilePort {
    public:
        /**
         * Creates a SerialPort given the /dev path
         *
         * @param path the path to the serial port
         */
        SerialPort(const std::string& path);

        virtual void flush() override;

        virtual void write(std::vector<uint8_t> data) override;

        static std::set<std::string> listPorts();

    private:
        int getFd(const std::string& path);
};


}} // end namespace cuauv::serial
