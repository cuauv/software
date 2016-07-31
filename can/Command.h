#pragma once

#include <cstdint>
#include <memory>

#include "lib/Message.h"
#include "lib/MessageBuilder.h"

class Command {
    public:

        virtual bool isValidResponse(const Message& msg) const = 0;
        virtual Message getMessage(std::shared_ptr<MessageBuilder> builder) const = 0;

        static std::shared_ptr<Command> setGroup(uint8_t devid, uint8_t gid);
        static std::shared_ptr<Command> setOffset(uint8_t devid, uint8_t offset);
        static std::shared_ptr<Command> setParam(uint8_t devid, uint8_t payload, uint16_t value);

    private:

};
