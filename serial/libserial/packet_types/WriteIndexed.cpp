#include "WriteIndexed.h"

namespace cuauv {
namespace serial {

WriteIndexedRequest::WriteIndexedRequest(const BoundRegisterSet& regs) : Request(Type::WriteIndexed), m_regs(regs) {

}

WriteIndexedRequest::WriteIndexedRequest(const std::vector<uint8_t>& request) : Request(Type::WriteIndexed) {
    auto payload = getPayload(request);
    if (payload.size() % 2 != 0) throw std::invalid_argument("Invalid WriteIndexed payload size");
    int num_regs = payload.size()/2;
    auto values_it = payload.begin()+num_regs;
    std::vector<uint8_t> indices(payload.begin(), values_it);
    std::vector<uint8_t> values(values_it, payload.end());
    m_regs = BoundRegisterSet(indices, values);
}

std::vector<uint8_t> WriteIndexedRequest::getBytes() const {
    auto packet = startPacket(2*m_regs.numRegisters());

    packet.insert(packet.end(), m_regs.key_begin(), m_regs.key_end()); // Add the register indices to the packet
    packet.insert(packet.end(), m_regs.value_begin(), m_regs.value_end()); // Add the register values to the packet

    finishPacket(packet);
    return packet;
}

bool WriteIndexedRequest::checkResponseHeader(const Header& header) const {
    if (header.type != Type::WriteIndexed) return false;
    if (header.len != 0) return false;
    return true;
}

std::shared_ptr<Response> WriteIndexedRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<WriteIndexedResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

BoundRegisterSet WriteIndexedRequest::getRegs() const {
    return m_regs;
}

WriteIndexedResponse::WriteIndexedResponse(const WriteIndexedRequest& UNUSED(request), const std::vector<uint8_t>& UNUSED(response)) : Response(Type::WriteIndexed) {
}

WriteIndexedResponse::WriteIndexedResponse() : Response(Type::WriteIndexed)
{
}

std::vector<uint8_t> WriteIndexedResponse::getBytes() const {
    auto packet = startPacket(0);

    finishPacket(packet);
    return packet;
}

}} // end namespace cuauv::serial
