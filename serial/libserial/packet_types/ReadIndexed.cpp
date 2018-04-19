#include "ReadIndexed.h"

namespace cuauv {
namespace serial {

ReadIndexedRequest::ReadIndexedRequest(const RegisterSet& regs) : Request(Type::ReadIndexed), m_regs(regs) {
    m_regOrder = std::vector<uint8_t>(m_regs.begin(), m_regs.end());
}

ReadIndexedRequest::ReadIndexedRequest(const std::vector<uint8_t>& request) : Request(Type::ReadIndexed), m_regs(getPayload(request)), m_regOrder(getPayload(request)) {
}

std::vector<uint8_t> ReadIndexedRequest::getBytes() const {
    auto packet = startPacket(m_regs.numRegisters());
    packet.insert(packet.end(), m_regs.begin(), m_regs.end());
    finishPacket(packet);
    return packet;
}

bool ReadIndexedRequest::checkResponseHeader(const Header& header) const {
    if (header.type != Type::ReadIndexed) return false;
    if (header.len != m_regs.numRegisters()) return false;
    return true;
}

std::shared_ptr<Response> ReadIndexedRequest::parseResponse(const std::vector<uint8_t>& response) const {
    try {
        return std::make_shared<ReadIndexedResponse>(*this, response);
    } catch (std::invalid_argument& e) {
        return nullptr;
    }
}

RegisterSet ReadIndexedRequest::getRegs() const {
    return m_regs;
}

std::vector<uint8_t> ReadIndexedRequest::getRegOrder() const {
    return m_regOrder;
}

ReadIndexedResponse::ReadIndexedResponse(const ReadIndexedRequest& request, const std::vector<uint8_t>& response) : Response(Type::ReadIndexed),
                m_regs(request.getRegOrder(), getPayload(response)),
                m_regOrder(request.getRegOrder())
{

}

ReadIndexedResponse::ReadIndexedResponse(const BoundRegisterSet& regs, const std::vector<uint8_t>& regOrder) : Response(Type::ReadIndexed),
           m_regs(regs),
           m_regOrder(regOrder)
{
    if (regOrder.size() != regs.numRegisters()) throw std::invalid_argument("Mismatched number of registers to ReadIndexedResponse");
}

std::vector<uint8_t> ReadIndexedResponse::getBytes() const {
    auto packet = startPacket(m_regs.numRegisters());

    for (auto reg : m_regOrder) {
        packet.push_back(m_regs[reg]);
    }

    finishPacket(packet);
    return packet;
}

BoundRegisterSet ReadIndexedResponse::getRegs() const {
    return m_regs;
}

std::vector<uint8_t> ReadIndexedResponse::getRegOrder() const {
    return m_regOrder;
}

}} // end namespace cuauv::serial
