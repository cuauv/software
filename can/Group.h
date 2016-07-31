#pragma once
#include <cstdint>

#include "lib/MessageData.h"

class Group {
    public:
        Group(uint8_t gid);

        uint8_t getGid() const;

        virtual void setSetpoint(uint8_t offset, int16_t setpoint);
        virtual int16_t getSetpoint(uint8_t offset) const;

        virtual MessageData buildMessageData() const;

    private:
        uint8_t m_gid;
        int16_t m_setpoints[4];
};
