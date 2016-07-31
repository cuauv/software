#include "Selector.h"
#include <stdexcept>

Selector::Selector(Type type, uint8_t value) : m_type(type), m_value(value) {
    if (m_value > 63) {
        throw std::invalid_argument("Selector value out of range");
    }
}

Selector::Selector(uint16_t field) {
    m_value = (uint8_t) (field & 0x003F);
    uint8_t typeField = (uint8_t) ((field >> 6) & 0x0007);
    switch (typeField) {
        case 0:
            m_type = Type::BROADCAST;
            break;
        case 1:
            m_type = Type::DEVID;
            break;
        case 2:
            m_type = Type::GROUP;
            break;
        case 3:
            m_type = Type::HWTYPE;
            break;
        case 4:
            m_type = Type::FWVER;
            break;
        default:
            throw std::domain_error("Bad selector type");
    }
}

uint16_t Selector::getField() const {
    return (((uint16_t) m_type) << 6) | m_value;
}

Selector::Type Selector::getType() const {
    return m_type;
}

uint8_t Selector::getValue() const {
    return m_value;
}

bool Selector::operator==(const Selector &other) const {
    if (m_type == Type::BROADCAST) {
        return other.m_type == Type::BROADCAST;
    } else {
        return (other.m_type == m_type) && (other.m_value == m_value);
    }
}

Selector Selector::Broadcast() {
    return Selector(Type::BROADCAST, 0);
}

Selector Selector::DevID(uint8_t id) {
    return Selector(Type::DEVID, id);
}

Selector Selector::Group(uint8_t group) {
    return Selector(Type::GROUP, group);
}

Selector Selector::HWType(uint8_t type) {
    return Selector(Type::HWTYPE, type);
}

Selector Selector::FWVer(uint8_t ver) {
    return Selector(Type::FWVER, ver);
}

std::string Selector::typeStr(Type type) {
    switch (type) {
        case Type::BROADCAST:
            return "Broadcast";
        case Type::DEVID:
            return "DevID";
        case Type::GROUP:
            return "Group";
        case Type::HWTYPE:
            return "HWType";
        case Type::FWVER:
            return "FWVer";
        default:
            return "";
    }
}

std::string Selector::str() const {
    auto type = typeStr(m_type);
    if (m_type == Type::BROADCAST) return type;
    else return type + " " + std::to_string(m_value);
}
