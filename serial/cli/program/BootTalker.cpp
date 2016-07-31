#include "BootTalker.h"

#include <chrono>
#include <iostream>

namespace cuauv {
namespace serial {
namespace cli {

BootTalker::BootTalker(std::unique_ptr<Port> port) : m_port(std::move(port))
{
}

std::string BootTalker::portName() const {
    return m_port->name();
}

std::vector<uint8_t> BootTalker::query(const std::vector<uint8_t>& request, unsigned int timeout, int len) {
    m_port->write(request);
    m_port->write({' '});

    auto end_time = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeout);
    auto remaining_time = [&] {
        return std::chrono::duration_cast<std::chrono::milliseconds>(end_time - std::chrono::steady_clock::now()).count();
    };

    auto start = m_port->read(1, remaining_time());
    if (start.size() != 1) {
        throw std::runtime_error("Device timeout");
    }
    if (start[0] != 0x14) {
        throw std::runtime_error("Invalid response start, got " + std::to_string(start[0]));
    }
    if (len < 0) {
        // dynamic sized data
        std::vector<uint8_t> data;
        while (1) {
            auto in = m_port->read(1, remaining_time());
            if (in.size() != 1) {
                throw std::runtime_error("Device timeout");
            }
            if (in[0] == 0x10) {
                return data;
            }
            data.push_back(in[0]);
        }
    } else {
        // static sized data
        auto data = m_port->read(len, remaining_time());
        if (data.size() != (unsigned int) len) {
            throw std::runtime_error("Device timeout");
        }
        auto end = m_port->read(1, remaining_time());
        if (end.size() != 1) {
            throw std::runtime_error("Device timeout");
        }
        if (end[0] != 0x10) {
            std::cout << "WARN: missing response terminator" << std::endl;
        }
        return data;
    }
}

}}} // end namespace cuauv::serial::cli
