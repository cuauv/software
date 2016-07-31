#include "ReadRange.h"

namespace cuauv {
namespace serial {

ReadRangeRequest::ReadRangeRequest(const RegisterRange& range) : Request(Type::ReadRange), m_range(range) {

}

ReadRangeRequest::ReadRangeRequest(const std::vector<uint8_t>& request) : Request(Type::ReadRange), m_range(0,0) {
    auto payload = getPayload(request);
    if (payload.size() != 2) throw std::invalid_argument("Invalid ReadRange payload size");
    m_range = RegisterRange(payload[1], payload[0]);
}

std::vector<uint8_t> ReadRangeRequest::getBytes() const {
    auto packet = startPacket(2);
    packet.push_back(m_range.numRegisters());
    packet.push_back(m_range.startRegister());
    finishPacket(packet);
    return packet;
}

bool ReadRangeRequest::checkResponseHeader(const Header& header) const {
    if (header.type != Type::ReadRange) return false;
    if (header.len != m_range.numRegisters()) return false;
    return true;
}

std::shared_ptr<Response> ReadRangeRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<ReadRangeResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

RegisterRange ReadRangeRequest::getRange() const {
    return m_range;
}

ReadRangeResponse::ReadRangeResponse(const ReadRangeRequest& request, const std::vector<uint8_t>& response) : Response(Type::ReadRange),
                                        m_range(request.getRange().startRegister(), getPayload(response))
{

}

ReadRangeResponse::ReadRangeResponse(const BoundRegisterRange& range) : Response(Type::ReadRange),
                            m_range(range)
{
}

std::vector<uint8_t> ReadRangeResponse::getBytes() const {
    auto packet = startPacket(m_range.numRegisters());

    packet.insert(packet.end(), m_range.begin(), m_range.end());

    finishPacket(packet);
    return packet;
}

BoundRegisterRange ReadRangeResponse::getRange() const {
    return m_range;
}

}} // end namespace cuauv::serial
