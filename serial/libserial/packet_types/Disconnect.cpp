#include "Disconnect.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

DisconnectRequest::DisconnectRequest() : Request(Type::Disconnect) {

}

DisconnectRequest::DisconnectRequest(const std::vector<uint8_t>& request) : Request(Type::Disconnect) {
    auto payload = getPayload(request);
    if (payload.size() != 0) throw std::invalid_argument("Invalid Disconnect payload size");
}

std::vector<uint8_t> DisconnectRequest::getBytes() const {
    auto packet = startPacket(0);
    finishPacket(packet);
    return packet;
}

bool DisconnectRequest::checkResponseHeader(const Header& UNUSED(header)) const {
    // Disconnect does not have a response
    return false;
}

std::shared_ptr<Response> DisconnectRequest::parseResponse(const std::vector<uint8_t>& UNUSED(response)) const {
    return nullptr;
}

}} // end namespace cuauv::serial
