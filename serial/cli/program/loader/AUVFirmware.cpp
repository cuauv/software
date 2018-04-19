#include "AUVFirmware.h"

namespace cuauv {
namespace serial {
namespace cli {

AUVFirmware::AUVFirmware(std::string deviceName, 
                    uint8_t sig1,
                    uint8_t sig2,
                    uint8_t sig3,
                    uint16_t pageSize,
                    const MemoryImage& mem) :
    m_deviceName(deviceName),
    m_sig1(sig1),
    m_sig2(sig2),
    m_sig3(sig3),
    m_pageSize(pageSize),
    m_mem(mem)
{
}

std::string AUVFirmware::deviceName() const {
    return m_deviceName;
}

uint8_t AUVFirmware::sig1() const {
    return m_sig1;
}

uint8_t AUVFirmware::sig2() const {
    return m_sig2;
}

uint8_t AUVFirmware::sig3() const {
    return m_sig3;
}

uint16_t AUVFirmware::pageSize() const {
    return m_pageSize;
}

MemoryImage AUVFirmware::mem() const {
    return m_mem;
}

}}} // end namespace cuauv::serial::cli
