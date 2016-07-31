#pragma once
#include <vector>
#include <iostream>
#include "MessageData.h"
#include "Selector.h"

class Message {
    public:
        enum class Type {
                        VALUE = 0x00,
                        OFFSET = 0x01,
                        PARAM = 0x02,
                        COMMIT = 0x03,
                        GROUP = 0x04,
                        ANNOUNCE = 0x05,
                        RESET = 0x06,
                        BL_ANNOUNCE = 0x10,
                        BL_PAGE = 0x11,
                        BL_DATA = 0x12,
                        BL_COMMIT = 0x13,
                        BL_PARAM_RESET = 0x14,
                        BL_GROUP = 0x15,
                        BL_RESET = 0x16
        };

        Message(Type type,
                uint8_t source,
                Selector selector,
                bool enable,
                uint8_t payload,
                bool rtr,
                MessageData data
                );

        Message(struct can_frame& frame);

        uint32_t getId() const;
        MessageData getData() const;
        void fillFrame(struct can_frame& frame) const;

        Type getType() const;
        uint8_t getSource() const;
        Selector getSelector() const;
        bool getEnable() const;
        bool isRTR() const;
        uint8_t getPayload() const;

        bool matchesAny(std::vector<Selector> selectors) const;

        static std::string typeStr(Type type);

        std::string str() const;

    private:
        Type m_type;
        uint8_t m_source;
        Selector m_selector;
        bool m_enable;
        uint8_t m_payload;
        bool m_rtr;
        const MessageData m_data;
};
