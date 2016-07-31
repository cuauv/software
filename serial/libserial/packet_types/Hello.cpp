#include "Hello.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

HelloRequest::HelloRequest() : Request(Type::Hello) {

}

HelloRequest::HelloRequest(const std::vector<uint8_t>& request) : Request(Type::Hello) {
    auto payload = getPayload(request);
    if (payload.size() != 0) throw std::invalid_argument("Invalid Hello payload size");
}

std::vector<uint8_t> HelloRequest::getBytes() const {
    auto packet = startPacket(0);
    finishPacket(packet);
    return packet;
}

bool HelloRequest::checkResponseHeader(const Header& header) const {
    // devInfo will generally be nullptr here
    if (header.type != Type::Hello) return false;
    if (header.len < 2) return false;
    return true;
}

std::shared_ptr<Response> HelloRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<HelloResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

HelloResponse::HelloResponse(const HelloRequest& UNUSED(request), const std::vector<uint8_t>& response) : Response(Type::Hello) {
    auto payload = getPayload(response);

    // getPayload will do size checking for us
    uint8_t protocolVersion = payload[0];
    uint8_t resetFlags = payload[1];
    std::string deviceConfig(payload.begin() + 2, payload.end());

    m_devInfo = std::make_shared<DeviceInfo>(protocolVersion, resetFlags, deviceConfig);
}

HelloResponse::HelloResponse(std::shared_ptr<DeviceInfo> devInfo) : Response(Type::Hello),
                    m_devInfo(devInfo)
{
}

std::vector<uint8_t> HelloResponse::getBytes() const {
    std::string cfg = m_devInfo->asString(); // Need config string here to know packet size

    auto packet = startPacket(2 + cfg.size());

    packet.push_back(m_devInfo->protocolVersion()); // protocol version
    packet.push_back(m_devInfo->resetFlags()); // reset flags
    packet.insert(packet.end(), cfg.begin(), cfg.end()); // device config

    finishPacket(packet);
    return packet;
}

uint8_t HelloResponse::getProtocolVersion() const {
    return m_devInfo->protocolVersion();
}

uint8_t HelloResponse::getResetFlags() const {
    return m_devInfo->resetFlags();
}

std::shared_ptr<DeviceInfo> HelloResponse::getDeviceInfo() const {
    return m_devInfo;
}

}} // end namespace cuauv::serial
