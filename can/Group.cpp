#include "Group.h"

#include <stdexcept>

Group::Group(uint8_t gid) :
                        m_gid(gid),
                        m_setpoints{0,0,0,0}
{
    if (gid > 63) throw std::domain_error("Invalid gid");
}

uint8_t Group::getGid() const {
    return m_gid;
}

void Group::setSetpoint(uint8_t offset, int16_t setpoint) {
    if (offset > 3) throw std::domain_error("Invalid offset");

    m_setpoints[offset] = setpoint;
}

int16_t Group::getSetpoint(uint8_t offset) const {
    if (offset > 3) throw std::domain_error("Invalid offset");

    return m_setpoints[offset];
}

MessageData Group::buildMessageData() const {
    MessageData data;
    for (uint8_t i=0; i < 4; i++) {
        data.int16Push(getSetpoint(i));
    }
    return data;
}
