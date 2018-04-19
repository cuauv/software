#pragma once

#include "lib/MessageBuilder.h"

class ShmMessageBuilder : public MessageBuilder {
    public:
        ShmMessageBuilder(uint8_t devid);

        virtual void setEnable(bool enable);
        virtual bool getEnable() const;
};
