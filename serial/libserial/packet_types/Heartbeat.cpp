#include "Heartbeat.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

HeartbeatRequest::HeartbeatRequest(bool softKill) : Request(Type::Heartbeat), m_softKill(softKill) {

}

HeartbeatRequest::HeartbeatRequest(const std::vector<uint8_t>& packet) : Request(Type::Heartbeat) {
    auto payload = getPayload(packet);

    if (payload.size() != 1) throw std::invalid_argument("Invalid Heartbeat payload size");

    m_softKill = payload[0];
}

std::vector<uint8_t> HeartbeatRequest::getBytes() const {
    auto packet = startPacket(1);
    packet.push_back(m_softKill ? 0x01 : 0x00);
    finishPacket(packet);
    return packet;
}

bool HeartbeatRequest::checkResponseHeader(const Header& header) const {
    if (header.type != Type::Heartbeat) return false;
    if (header.len != 3) return false;
    return true;
}

std::shared_ptr<Response> HeartbeatRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<HeartbeatResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

bool HeartbeatRequest::getSoftKill() const {
    return m_softKill;
}

HeartbeatResponse::HeartbeatResponse(const HeartbeatRequest& UNUSED(request), const std::vector<uint8_t>& response) : Response(Type::Heartbeat) {
    auto payload = getPayload(response);

    m_hardKillValid = (payload[0] & 0x02) ? true : false;
    m_hardKill = (payload[0] & 0x01) ? true : false;
    m_heartbeatTime = ((uint16_t)payload[2]) << 8 | payload[1];
}

HeartbeatResponse::HeartbeatResponse(bool hardKill, bool hardKillValid, uint16_t heartbeatTime) :
    Response(Type::Heartbeat),
    m_hardKill(hardKill),
    m_hardKillValid(hardKillValid),
    m_heartbeatTime(heartbeatTime)
{
}

std::vector<uint8_t> HeartbeatResponse::getBytes() const {
    auto packet = startPacket(3);

    uint8_t killByte = 0;
    if (m_hardKill) killByte |= 0x01;
    if (m_hardKillValid) killByte |= 0x02;
    packet.push_back(killByte);

    packet.push_back(m_heartbeatTime & 0x00FF);
    packet.push_back(m_heartbeatTime >> 8);

    finishPacket(packet);
    return packet;
}

bool HeartbeatResponse::getHardKill() const {
    return m_hardKill;
}

bool HeartbeatResponse::isHardKillValid() const {
    return m_hardKillValid;
}

uint16_t HeartbeatResponse::getHeartbeatTime() const {
    return m_heartbeatTime;
}

}} // end namespace cuauv::serial
