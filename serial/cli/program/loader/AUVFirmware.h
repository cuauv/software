#pragma once

#include <string>
#include <inttypes.h>

#include "MemoryImage.h"

namespace cuauv {
namespace serial {
namespace cli {

/**
 * @brief A complete AUV firmware object
 */
class AUVFirmware {
    public:
        /**
         * Creates a firmware object
         *
         * @param deviceName the name of the device this firmware is intended for
         * @param sig1 the SIG1 byte of the intended device
         * @param sig2 the SIG2 byte of the intended device
         * @param sig3 the SIG3 byte of the intended device
         * @param pageSize the page size of the intended device
         * @param mem the memory image for this firmware
         */
        AUVFirmware(std::string deviceName, 
                    uint8_t sig1,
                    uint8_t sig2,
                    uint8_t sig3,
                    uint16_t pageSize,
                    const MemoryImage& mem);

        std::string deviceName() const;
        uint8_t sig1() const;
        uint8_t sig2() const;
        uint8_t sig3() const;
        uint16_t pageSize() const;
        MemoryImage mem() const;

    private:
        const std::string m_deviceName;
        const uint8_t m_sig1;
        const uint8_t m_sig2;
        const uint8_t m_sig3;
        const uint16_t m_pageSize;
        const MemoryImage m_mem;
};

}}} // end namespace cuauv::serial::cli
