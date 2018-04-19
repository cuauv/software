#include "MessageData.h"
#include <stdexcept>

MessageData::MessageData() 
{
}

MessageData::MessageData(std::initializer_list<uint8_t> l) :
                             m_data(l)
{
    if (m_data.size() > 8) throw std::invalid_argument("MessageData too big");
}

MessageData::MessageData(struct can_frame& frame) :
                             m_data(frame.data, frame.data + frame.can_dlc)
{
}

void MessageData::fillFrame(struct can_frame& frame) const {
    frame.can_dlc = size();
    for (int i = 0; i < frame.can_dlc; i++) {
        frame.data[i] = m_data[i];
    }
}

uint8_t MessageData::size() const {
    return m_data.size();
}

bool MessageData::empty() const {
    return m_data.size() == 0;
}

uint8_t& MessageData::operator[](uint8_t pos) {
    return m_data[pos];
}

const uint8_t& MessageData::operator[](uint8_t pos) const {
    return m_data[pos];
}

void MessageData::uint8Push(uint8_t data) {
    if (m_data.size() > 7) throw std::length_error("Can't push uint8 to MessageData");

    m_data.push_back(data);
}

uint8_t MessageData::uint8Pop() {
    if (m_data.size() < 1) throw std::length_error("Can't pop uint8 from MessageData");

    uint8_t data = m_data.back();
    m_data.pop_back();
    return data;
}

void MessageData::int16Push(int16_t data) {
    if (m_data.size() > 6) throw std::length_error("Can't push int16 to MessageData");

    uint8_t part = (uint8_t) ((data & 0xFF00) >> 8);
    m_data.push_back(part);
    part = (uint8_t) (data & 0x00FF);
    m_data.push_back(part);
}

int16_t MessageData::int16Pop() {
    if (m_data.size() < 2) throw std::length_error("Can't pop int16 from MessageData");

    int16_t result = (int16_t) m_data.back();
    m_data.pop_back();
    result |= ((int16_t) m_data.back()) << 8;
    m_data.pop_back();
    return result;
}

std::string MessageData::str() const {
    std::string result = "";
    for (auto val : m_data) {
        result += std::to_string(val) + " ";
    }
    return result;
}
