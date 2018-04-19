#include "Reset.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

ResetRequest::ResetRequest() : Request(Type::Reset) {

}

ResetRequest::ResetRequest(const std::vector<uint8_t>& request) : Request(Type::Reset) {
    auto payload = getPayload(request);
    if (payload.size() != 0) throw std::invalid_argument("Invalid Reset request payload size");
}

std::vector<uint8_t> ResetRequest::getBytes() const {
    auto packet = startPacket(0);
    finishPacket(packet);
    return packet;
}

bool ResetRequest::checkResponseHeader(const Header& UNUSED(header)) const {
    // Reset does not have a response
    return false;
}

std::shared_ptr<Response> ResetRequest::parseResponse(const std::vector<uint8_t>& UNUSED(response)) const {
    return nullptr;
}

}} // end namespace cuauv::serial
