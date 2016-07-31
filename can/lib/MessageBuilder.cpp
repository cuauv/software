#include <stdexcept>
#include "MessageBuilder.h"

MessageBuilder::MessageBuilder(uint8_t devid) :
                    m_devid(devid),
                    m_enable(false)
{
    if (m_devid > 63) throw std::domain_error("Invalid DevID");
}

void MessageBuilder::setEnable(bool enable) {
    m_enable = enable;
}

bool MessageBuilder::getEnable() const {
    return m_enable;
}

Message MessageBuilder::buildMsg(Message::Type type,
                                 Selector selector,
                                 uint8_t payload,
                                 bool rtr,
                                 MessageData data) const {
    return Message(type,
                   m_devid,
                   selector,
                   getEnable(),
                   payload,
                   rtr,
                   data);
}
