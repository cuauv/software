#pragma once

#include <memory>

#include <Port.h>

namespace cuauv {
namespace serial {
namespace cli {

class BootTalker {
    public:
        BootTalker(std::unique_ptr<Port> port);

        std::string portName() const;

        std::vector<uint8_t> query(const std::vector<uint8_t>& request, unsigned int timeout, int len = -1);

    private:
        std::unique_ptr<Port> m_port;
};

}}} // end namespace cuauv::serial::cli
