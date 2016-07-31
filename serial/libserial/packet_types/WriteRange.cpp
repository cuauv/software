#include "WriteRange.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

WriteRangeRequest::WriteRangeRequest(const BoundRegisterRange& range) : Request(Type::WriteRange), m_range(range) {

}

WriteRangeRequest::WriteRangeRequest(const std::vector<uint8_t>& request) : Request(Type::WriteRange), m_range(0, {}) {
    auto payload = getPayload(request);
    if (payload.size() < 1) throw std::invalid_argument("Invalid WriteRange request size");
    std::vector<uint8_t> values(payload.begin()+1, payload.end());
    m_range = BoundRegisterRange(payload[0], values);
}

std::vector<uint8_t> WriteRangeRequest::getBytes() const {
    auto packet = startPacket(1 + m_range.numRegisters());
    packet.push_back(m_range.startRegister());
    packet.insert(packet.end(), m_range.begin(), m_range.end());
    finishPacket(packet);
    return packet;
}

bool WriteRangeRequest::checkResponseHeader(const Header& header) const {
    if (header.type != Type::WriteRange) return false;
    if (header.len != 0) return false;
    return true;
}

std::shared_ptr<Response> WriteRangeRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<WriteRangeResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

BoundRegisterRange WriteRangeRequest::getRange() const {
    return m_range;
}

WriteRangeResponse::WriteRangeResponse(const WriteRangeRequest& UNUSED(request), const std::vector<uint8_t>& UNUSED(response)) : Response(Type::WriteRange) {
}

WriteRangeResponse::WriteRangeResponse() : Response(Type::WriteRange)
{

}

std::vector<uint8_t> WriteRangeResponse::getBytes() const {
    auto packet = startPacket(0);

    finishPacket(packet);
    return packet;
}

}} // end namespace cuauv::serial
