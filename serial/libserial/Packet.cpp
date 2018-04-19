#include "Packet.h"

#include "Log.h"

namespace cuauv {
namespace serial {

Packet::~Packet() {
}

Packet::Packet(Type type, bool isResponse) : m_type(type), m_isResponse(isResponse)
{
}

Packet::Header Packet::parseHeader(const std::vector<uint8_t>& header) {
    if (header.size() < 3) throw std::invalid_argument("Header length " + std::to_string(header.size()) + " too short");

    Header hdr;
    // Switch on packet type field
    // Doing it this way has to check that the packet's type field is a valid type
    switch ((Type) header[0]) {
        case Type::Hello:
            hdr.type = Type::Hello;
            break;
        case Type::Reset:
            hdr.type = Type::Reset;
            break;
        case Type::Heartbeat:
            hdr.type = Type::Heartbeat;
            break;
        case Type::ReadRange:
            hdr.type = Type::ReadRange;
            break;
        case Type::WriteRange:
            hdr.type = Type::WriteRange;
            break;
        case Type::ReadIndexed:
            hdr.type = Type::ReadIndexed;
            break;
        case Type::WriteIndexed:
            hdr.type = Type::WriteIndexed;
            break;
        case Type::Disconnect:
            hdr.type = Type::Disconnect;
            break;
        case Type::Error:
            hdr.type = Type::Error;
            break;
        default:
            throw std::invalid_argument("Invalid packet type " + std::to_string(header[0]));
    }

    hdr.len = (((uint16_t) header[2]) << 8) | header[1];

    return hdr;
}

std::vector<uint8_t> Packet::startPacket(uint16_t len) const {
    std::vector<uint8_t> packet;
    packet.reserve(len + 4);

    packet.push_back((uint8_t) m_type); // Packet type
    packet.push_back(len & 0x00FF); // LSB of length
    packet.push_back(len >> 8); // MSB of length

    return packet;
}

void Packet::finishPacket(std::vector<uint8_t>& packet) const {
    auto header = parseHeader(packet);
    if (header.len != packet.size() - 3) throw std::invalid_argument("Improperly sized packet payload");
    uint8_t checksum = 0;
    for (auto packetByte : packet) {
        checksum += packetByte;
    }
    packet.push_back(checksum);
}

bool Packet::checkPacket(const std::vector<uint8_t>& packet) const {
    if (packet.size() < 3) {
        Log::warn(m_port, "Packet of size " + std::to_string(packet.size()) + " is too short");
        return false; // packet isn't even large enough for a header
    }

    auto header = parseHeader(packet);
    if (header.len != packet.size() - 4) {
        Log::warn(m_port, "Packet payload size mismatch (header says " + std::to_string(header.len) + " bytes, received " + std::to_string(packet.size() - 4) + " bytes)");
        return false; // payload size mismatch
    }
    if (header.type != m_type) {
        Log::warn(m_port, "Packet type mismatch (got " + std::to_string((uint8_t) header.type) + ", expecting " + std::to_string((uint8_t) m_type) + ")");
        return false; // type mismatch
    }

    uint8_t checksum = 0;
    for (auto packetByte : packet) {
        checksum += packetByte;
    }
    auto packet_checksum = packet[packet.size()-1];
    checksum -= packet_checksum; // checksum byte is not part of the checksum, un-add it here
    if (checksum != packet_checksum){
        Log::warn(m_port, "Invalid packet checksum (computed " + std::to_string(checksum) + ", packet says " + std::to_string(packet_checksum) + ")");
        return false; // checksum mismatch
    }

    return true;
}

std::vector<uint8_t> Packet::getPayload(const std::vector<uint8_t>& packet) const {
    if (!checkPacket(packet)) throw std::invalid_argument("Getting payload of an invalid packet");
    auto header = parseHeader(packet);
    std::vector<uint8_t> payload(packet.begin()+3, packet.begin()+3+header.len); // Copy the payload out of the packet
    return payload;
}

void Packet::setPort(const std::string& port) {
    m_port = port;
}

Packet::Type Packet::type() const {
    return m_type;
}

bool Packet::isResponse() const {
    return m_isResponse;
}

Response::Response(Type type) : Packet(type, true) {

}

Request::Request(Type type) : Packet(type, false) {

}

}} // end namespace cuauv::serial
