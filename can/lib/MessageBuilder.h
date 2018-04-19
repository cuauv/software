#pragma once

#include "Message.h"

class MessageBuilder {
    public:
        MessageBuilder(uint8_t devid);

        virtual void setEnable(bool enable);
        virtual bool getEnable() const;

        Message buildMsg(Message::Type type,
                         Selector selector,
                         uint8_t payload,
                         bool rtr,
                         MessageData data
                ) const;

    private:
        const uint8_t m_devid;
        bool m_enable;
};
