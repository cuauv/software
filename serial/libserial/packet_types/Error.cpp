#include "Error.h"

namespace cuauv {
namespace serial {

ErrorResponse::ErrorResponse(const std::vector<uint8_t>& response) : Response(Type::Error) {
    auto payload = getPayload(response);
    
    // We don't have a request type, so less length checking is done ahead of time
    if (payload.size() != 1) throw std::invalid_argument("Invalid error response payload size");

    m_error = (Error) payload[0];
}

ErrorResponse::ErrorResponse(Error error) : Response(Type::Error), m_error(error)
{

}

std::vector<uint8_t> ErrorResponse::getBytes() const {
    auto packet = startPacket(1);

    packet.push_back((uint8_t) m_error);

    finishPacket(packet);
    return packet;
}

ErrorResponse::Error ErrorResponse::getError() const {
    return m_error;
}

std::string ErrorResponse::getErrorString() const {
    switch (m_error) {
        case Error::TYPE:
            return "Type";
        case Error::CHECKSUM:
            return "Checksum";
        case Error::RANGE:
            return "Range";
        case Error::LENGTH:
            return "Length";
        default:
            return std::to_string((uint8_t) m_error);
    }
}

}} // end namespace cuauv::serial
