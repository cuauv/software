#include "DeviceTalker.h"

#include <packet_types/All.h>

#include <chrono>

#define T_BYTE 5
#define T_REPLY 20

// Chunk size is a minor breach of what the serial spec requires
// In a 100% proper implementation, the host would have a T_BYTE timeout
// between each individual byte in the message. However, using such a scheme
// on a real OS would mean a syscall on each byte read, which seems a bit much.
// So instead, the talker splits reads up into chunks, and gives each chunk a timeout
// of READ_CHUNK_SIZE*T_BYTE. The downside of this is that timeouts take much longer to
// detect. Luckily, each read begins with a 3 byte chunk for the header, so the only
// time a full size chunk could time out across multiple replies is if the header's size
// field was reliably corrupted.
#define READ_CHUNK_SIZE 25

namespace cuauv {
namespace serial {
namespace cli {

DeviceTalker::DeviceTalker(std::unique_ptr<Port> port) : m_port(std::move(port))
{
}

std::string DeviceTalker::portName() const {
    return m_port->name();
}

void DeviceTalker::send(std::shared_ptr<Response> response) {
    m_port->write(response->getBytes());
}

std::shared_ptr<Request> DeviceTalker::listen(unsigned int timeout) {
    auto end_time = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeout);
    auto now = std::chrono::steady_clock::now();
    while (end_time > now) {
        auto remaining_duration = end_time - now;
        unsigned int remaining_timeout = std::chrono::duration_cast<std::chrono::milliseconds>(remaining_duration).count();
        
        auto packet = m_port->read(1, remaining_timeout);
        if (packet.size() != 0) {
            auto remaining_header = m_port->read(2, 2*T_BYTE);
            if (remaining_header.size() == 2) {
                packet.insert(packet.end(), remaining_header.begin(), remaining_header.end());
                try {
                    auto header = Packet::parseHeader(packet);
                    if (header.type != Packet::Type::Error) {
                        readChunked(packet, header.len+1);
                        auto request = parseRequest(packet);
                        if (request) {
                            return request;
                        } else {
                            waitHostIdle();
                        }
                    } else {
                        waitHostIdle();
                    }
                } catch (std::exception& e) {
                    waitHostIdle();
                }
            }
        }

        // update the timer
        now = std::chrono::steady_clock::now();
    }

    return nullptr;
}

std::shared_ptr<Request> DeviceTalker::parseRequest(const std::vector<uint8_t>& request) {
    try {
        auto header = Packet::parseHeader(request);
        switch (header.type) {
            case Packet::Type::Hello:
                return std::make_shared<HelloRequest>(request);
            case Packet::Type::Reset:
                return std::make_shared<ResetRequest>(request);
            case Packet::Type::Heartbeat:
                return std::make_shared<HeartbeatRequest>(request);
            case Packet::Type::ReadRange:
                return std::make_shared<ReadRangeRequest>(request);
            case Packet::Type::WriteRange:
                return std::make_shared<WriteRangeRequest>(request);
            case Packet::Type::ReadIndexed:
                return std::make_shared<ReadIndexedRequest>(request);
            case Packet::Type::WriteIndexed:
                return std::make_shared<WriteIndexedRequest>(request);
            case Packet::Type::Disconnect:
                return std::make_shared<DisconnectRequest>(request);
            default:
                return nullptr;
        }
    } catch (std::exception& e) {
        return nullptr;
    }
}

bool DeviceTalker::readChunked(std::vector<uint8_t>& message_bytes, uint16_t count) {
    message_bytes.reserve(message_bytes.size() + count);

    while (count > 0) {
        uint16_t chunk_size = std::min(count, (uint16_t) READ_CHUNK_SIZE);
        count -= chunk_size;

        auto chunk = m_port->read(chunk_size, T_BYTE * chunk_size);
        if (chunk.size() != chunk_size) return false; // Chunk timed out
        message_bytes.insert(message_bytes.end(), chunk.begin(), chunk.end()); // append the chunk to the message
    }

    return true;
}

void DeviceTalker::waitHostIdle() {
    bool finished = false;

    while (!finished) {
        auto data = m_port->read(5, 5 * T_BYTE);
        if (data.size() < 5) finished = true;
    }
}

}}} // end namespace cuauv::serial::cli
