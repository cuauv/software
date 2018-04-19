#include "FakeDev.h"

#include <can/lib/Log.h>

FakeDev::FakeDev(uint8_t devid, uint8_t hwtype, uint8_t fwver) :
                                                    m_devid(devid),
                                                    m_group(0),
                                                    m_hwtype(hwtype),
                                                    m_fwver(fwver),
                                                    m_value(0),
                                                    m_offset(0),
                                                    m_sEnable(false),
                                                    m_hEnable(false)
{
    m_selectors.reserve(5);
    m_selectors.push_back(Selector::Broadcast());
    m_selectors.push_back(Selector::DevID(devid));
    m_selectors.push_back(Selector::Group(0));
    m_selectors.push_back(Selector::HWType(hwtype));
    m_selectors.push_back(Selector::FWVer(fwver));

    for (int i=0; i < NUM_PARAM; i++) {
        m_params[i] = 0;
    }
}

void FakeDev::setGroup(uint8_t gid) {
    m_group = gid;
    m_selectors[2] = Selector::Group(gid);
    if (gid == 0) {
        m_hEnable = false;
        m_value = 0;
    }
    Log::log("Setting group to " + std::to_string(gid));
}

void FakeDev::setHEnable(bool hEnable) {
    if (m_group == 0) hEnable = false;
    m_hEnable = hEnable;
}

uint8_t FakeDev::getGroup() const {
    return m_group;
}

uint8_t FakeDev::getDevID() const {
    return m_devid;
}

uint8_t FakeDev::getHWType() const {
    return m_hwtype;
}

uint8_t FakeDev::getFWVer() const {
    return m_fwver;
}

int16_t FakeDev::getValue() const {
    return m_value;
}

uint8_t FakeDev::getOffset() const {
    return m_offset;
}

bool FakeDev::getSEnable() const {
    return m_sEnable && m_group != 0 && m_group != 1;
}

bool FakeDev::getHEnable() const {
    return m_hEnable;
}

std::unique_ptr<Message> FakeDev::handleMessage(Message& msg) {
    if (msg.matchesAny(m_selectors)) {
        m_sEnable = msg.getEnable();
        auto data = msg.getData();
        switch(msg.getType()) {
            case Message::Type::VALUE:
                if(!msg.isRTR()) {
                    // hax lol
                    int16_t val = 0;
                    for (uint8_t i = 4-m_offset; i > 0; i--) {
                        val = data.int16Pop();
                    }
                    m_value = val;
                }
                return valueMessage();
            case Message::Type::OFFSET:
                if(!msg.isRTR()) {
                    m_offset = msg.getPayload();
                    if (m_offset > 3) throw std::domain_error("Invalid offset");
                    Log::log("Setting offset to " + std::to_string(m_offset));
                }
                return offsetMessage();
            case Message::Type::PARAM:
                if (!msg.isRTR()) {
                    int paramId = msg.getPayload();
                    if (paramId < NUM_PARAM) m_params[paramId] = data.int16Pop();
                }
                return paramMessage(msg.getPayload());
            case Message::Type::COMMIT:
                return commitMessage(msg.getPayload());
            case Message::Type::GROUP:
                if (!msg.isRTR()) {
                    setGroup(msg.getPayload());
                }
                return groupMessage();
            case Message::Type::ANNOUNCE:
                Log::log("Responding to Announce message");
                return announceMessage();
            case Message::Type::RESET:
                return nullptr;
            default:
                return nullptr;
        }
    }
    return nullptr;
}

bool FakeDev::wantsMessage(Message& msg) const {
    return msg.matchesAny(m_selectors);
}

std::unique_ptr<Message> FakeDev::valueMessage() const {
    MessageData data;
    data.int16Push(m_value);
    data.int16Push(0);
    data.int16Push(0);
    data.int16Push(0);
    return std::unique_ptr<Message> (new Message(Message::Type::VALUE, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   0,
                   false,
                   data
                ));
}

std::unique_ptr<Message> FakeDev::offsetMessage() const {
    return std::unique_ptr<Message> (new Message(Message::Type::OFFSET, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   m_offset,
                   false,
                   {}
                ));
}

std::unique_ptr<Message> FakeDev::paramMessage(uint8_t id) const {
    if (id >= NUM_PARAM) throw std::domain_error("Invalid param id");
    MessageData data;
    data.int16Push(m_params[id]);
    return std::unique_ptr<Message> (new Message(Message::Type::PARAM, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   id,
                   false,
                   data
                ));
}

std::unique_ptr<Message> FakeDev::commitMessage(uint8_t id) const {
    if (id >= NUM_PARAM) throw std::domain_error("Invalid param id");
    return std::unique_ptr<Message> (new Message(Message::Type::COMMIT, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   id,
                   false,
                   {}
                ));
}

std::unique_ptr<Message> FakeDev::groupMessage() const {
    return std::unique_ptr<Message> (new Message(Message::Type::GROUP, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   m_group,
                   false,
                   {}
                ));
}

std::unique_ptr<Message> FakeDev::announceMessage() const {
    return std::unique_ptr<Message> (new Message(Message::Type::ANNOUNCE, 
                   m_devid,
                   Selector::DevID(0),
                   m_hEnable,
                   0,
                   false,
                   {m_group, m_offset, m_hwtype, m_fwver}
                ));
}
