#include "Message.h"
#include <stdexcept>
#include <algorithm>
#include <linux/can/raw.h>

Message::Message(Type type, uint8_t source, Selector selector, bool enable, uint8_t payload, bool rtr, MessageData data) :
                m_type(type),
                m_source(source),
                m_selector(selector),
                m_enable(enable),
                m_payload(payload),
                m_rtr(rtr),
                m_data(data)
{
    if (m_rtr && !m_data.empty()) {
        throw std::invalid_argument("Data in RTR Message");
    }

    if (m_data.size() > 8) {
        throw std::invalid_argument("Message data range too big");
    }

    if (m_source > 63) {
        throw std::invalid_argument("Message source id too big");
    }
}

Message::Message(struct can_frame& frame) :
                m_selector(Selector::Broadcast()), // Selector doesn't have a default constructor
                m_data(frame)
{
    uint32_t id = frame.can_id;
    if (m_data.size() > 8) {
        throw std::invalid_argument("Message data too big");
    }
    if (!(id & CAN_EFF_FLAG)) {
        throw std::domain_error("Message is not an extended frame");
    }
    if (id & CAN_RTR_FLAG) {
        m_rtr = true;
        if (!m_data.empty()) throw std::invalid_argument("Data in RTR Message");
    } else {
        m_rtr = false;
    }
    id = id & CAN_EFF_MASK;

    m_payload = (uint8_t) (id & 0x000000FF);
    id = id >> 8;
    m_enable = (id & 0x00000001) ? true : false;
    id = id >> 1;
    m_selector = Selector((uint16_t) (id & 0x000001FF));
    id = id >> 9;
    m_source = (uint8_t) (id & 0x0000003F);
    id = id >> 6;
    uint8_t type = (uint8_t) (id & 0x0000001F);
    switch (type) {
        case 0x00:
            m_type = Type::VALUE;
            break;
        case 0x01:
            m_type = Type::OFFSET;
            break;
        case 0x02:
            m_type = Type::PARAM;
            break;
        case 0x03:
            m_type = Type::COMMIT;
            break;
        case 0x04:
            m_type = Type::GROUP;
            break;
        case 0x05:
            m_type = Type::ANNOUNCE;
            break;
        case 0x06:
            m_type = Type::RESET;
            break;
        case 0x10:
            m_type = Type::BL_ANNOUNCE;
            break;
        case 0x11:
            m_type = Type::BL_PAGE;
            break;
        case 0x12:
            m_type = Type::BL_DATA;
            break;
        case 0x13:
            m_type = Type::BL_COMMIT;
            break;
        case 0x14:
            m_type = Type::BL_PARAM_RESET;
            break;
        case 0x15:
            m_type = Type::BL_GROUP;
            break;
        case 0x16:
            m_type = Type::BL_RESET;
            break;
        default:
            throw std::domain_error("Bad message type in packet");
    }
    
}

uint32_t Message::getId() const {
    uint32_t id = m_payload;

    if (m_enable) {
        id |= 0x00000100;
    }

    id |= ((uint32_t) m_selector.getField()) << 9;

    id |= ((uint32_t) m_source) << 18;

    id |= ((uint32_t) m_type) << 24;

    id |= CAN_EFF_FLAG;

    if (m_rtr) id |= CAN_RTR_FLAG;

    return id;
}

MessageData Message::getData() const {
    return m_data;
}

void Message::fillFrame(struct can_frame& frame) const {
    frame.can_id = getId();
    m_data.fillFrame(frame);
}

Message::Type Message::getType() const {
    return m_type;
}

uint8_t Message::getSource() const {
    return m_source;
}

Selector Message::getSelector() const {
    return m_selector;
}

bool Message::getEnable() const {
    return m_enable;
}

uint8_t Message::getPayload() const {
    return m_payload;
}

bool Message::isRTR() const {
    return m_rtr;
}

bool Message::matchesAny(std::vector<Selector> selectors) const {
    return std::any_of(selectors.begin(), selectors.end(), [&](const Selector &sel) {return m_selector == sel;});
}

std::string Message::typeStr(Type type) {
    switch (type) {
    case Type::VALUE:
        return "Value";
    case Type::OFFSET:
        return "Offset";
    case Type::PARAM:
        return "Param";
    case Type::COMMIT:
        return "Commit";
    case Type::GROUP:
        return "Group";
    case Type::ANNOUNCE:
        return "Announce";
    case Type::RESET:
        return "Reset";
    case Type::BL_ANNOUNCE:
        return "BL Announce";
    case Type::BL_PAGE:
        return "BL Page";
    case Type::BL_DATA:
        return "BL Data";
    case Type::BL_COMMIT:
        return "BL Commit";
    case Type::BL_PARAM_RESET:
        return "BL Param Reset";
    case Type::BL_GROUP:
        return "BL Group";
    case Type::BL_RESET:
        return "BL Reset";
    default:
        return "";
    }
}

std::string Message::str() const {
    auto message = typeStr(m_type);
    if (m_rtr) message += " [RTR]";
    message += " from " + std::to_string(m_source) + " to " + m_selector.str();
    message += ". Status: ";
    if (m_enable)
        message += "Enable";
    else
        message += "Disable";
    message += ", payload: " + std::to_string(m_payload);

    if (!m_data.empty()) {
        message += ", data: " + m_data.str();
    }

    return message;
}
