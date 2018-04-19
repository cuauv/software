#pragma once
#include <cstdint>
#include <vector>
#include <initializer_list>
#include <string>

#include <linux/can/raw.h>

class MessageData {
    public:
        MessageData();
        MessageData(std::initializer_list<uint8_t> l);
        MessageData(struct can_frame& frame);

        void fillFrame(struct can_frame& frame) const;

        uint8_t size() const;
        bool empty() const;

        uint8_t& operator[](uint8_t pos);
        const uint8_t& operator[](uint8_t pos) const;

        void uint8Push(uint8_t data);
        uint8_t uint8Pop();

        void int16Push(int16_t data);
        int16_t int16Pop();

        std::string str() const;

    private:
        std::vector<uint8_t> m_data;

};
