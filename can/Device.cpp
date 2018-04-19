#include "Device.h"

#include <stdexcept>

Device::Device(uint8_t devid, uint8_t desiredGroup, uint8_t desiredOffset) :
                            m_devid(devid),
                            m_hwtype(0),
                            m_fwver(0),
                            m_group(0),
                            m_offset(0),
                            m_feedback{0,0,0,0},
                            m_flags(0),
                            m_curCommand(nullptr),
                            m_lastMsgTime(std::chrono::steady_clock::now()),
                            m_cmdStartTime(std::chrono::steady_clock::now())
{
    if (devid > 63 || devid == 0) throw std::domain_error("Invalid devid");
    // Messages MUST go in this order (device could go active with wrong offset
    // otherwise
    pushCommand(Command::setOffset(m_devid, desiredOffset));
    pushCommand(Command::setGroup(m_devid, desiredGroup));
}

Device::~Device() {
    onDeviceCulled();
}

uint8_t Device::getDevID() const {
    return m_devid;
}

uint8_t Device::getHWType() const {
    return m_hwtype;
}

uint8_t Device::getFWVer() const {
    return m_fwver;
}

uint8_t Device::getGroup() const {
    return m_group;
}

uint8_t Device::getOffset() const {
    return m_offset;
}

bool Device::getHardEnable() const {
    return m_hardEnable;
}

int16_t Device::getFeedback(uint8_t id) const {
    if (id > 3) throw std::domain_error("Invalid Feedback slot id");
    return m_feedback[id];
}

uint8_t Device::getFlags() const {
    return m_flags;
}

void Device::processMessage(const Message& msg, const CANSocket& sock, std::shared_ptr<MessageBuilder> builder) {
    if (msg.getType() == Message::Type::VALUE) {
        MessageData data = msg.getData();
        m_feedback[3] = data.int16Pop();
        m_feedback[2] = data.int16Pop();
        m_feedback[1] = data.int16Pop();
        m_feedback[0] = data.int16Pop();
        m_flags = msg.getPayload();
        onStatusChanged();
    } else if (msg.getType() == Message::Type::ANNOUNCE) {
        MessageData data = msg.getData();
        m_fwver = data.uint8Pop();
        m_hwtype = data.uint8Pop();
        m_offset = data.uint8Pop();
        m_group = data.uint8Pop();
        onDeviceAnnounced();
    }

    m_hardEnable = msg.getEnable();

    m_lastMsgTime = std::chrono::steady_clock::now();

    {
        // Need to make sure cmdMutex is not held during onDeviceAnnounced() call
        std::lock_guard<std::mutex> lck(m_cmdMutex);
        if (m_curCommand) {
            if (m_curCommand->isValidResponse(msg)) {
                MessageData data = msg.getData();
                switch (msg.getType()) {
                    case Message::Type::OFFSET:
                        m_offset = msg.getPayload();
                        break;
                    case Message::Type::GROUP:
                        m_group = msg.getPayload();
                        break;
                    default:
                        break;
                }

                m_curCommand = nullptr;
            }
        }

        if (!m_curCommand && !m_cmdQueue.empty()) {
            m_curCommand = m_cmdQueue.front();
            m_cmdQueue.pop_front();

            sock.sendMessage(m_curCommand->getMessage(builder));
            m_cmdStartTime = std::chrono::steady_clock::now();
        }
    }
}

void Device::pushCommand(std::shared_ptr<Command> cmd) {
    std::lock_guard<std::mutex> lck(m_cmdMutex);
    m_cmdQueue.push_back(cmd);
}

void Device::clearCommands() {
    std::lock_guard<std::mutex> lck(m_cmdMutex);
    m_cmdQueue.clear();
}

bool Device::isDead() {
    std::lock_guard<std::mutex> lck(m_cmdMutex);

    auto now = std::chrono::steady_clock::now();
    if (m_lastMsgTime + std::chrono::milliseconds(500) < now) return true;
    if (m_curCommand != nullptr && m_cmdStartTime + std::chrono::milliseconds(500) < now) return true;

    return false;
}

void Device::onDeviceAnnounced() {
    // nothing to see here
}

void Device::onStatusChanged() {
    // Nothing to see here
}

void Device::onDeviceCulled() {
    // Nothing to see here
}
