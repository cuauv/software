#include "Talker.h"

#include <thread>
#include <chrono>
#include <stdexcept>
#include <algorithm>

#include "packet_types/Error.h"
#include "Log.h"

// These constants are pulled from the serial protocol spec
#define RETRY_COUNT 3
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

Talker::Talker(std::unique_ptr<Port> port) : m_port(std::move(port)) {
    
}

std::string Talker::portName() const {
    return m_port->name();
}

std::shared_ptr<Response> Talker::query(std::shared_ptr<Request> request) {
    request->setPort(portName());

    int retryCount = 0;
    do {
        retryCount++;

        // send will flush for us
        send(request);
        auto message_bytes = m_port->read(3, T_REPLY + 3*T_BYTE); // We need to wait for a full reply time, plus 
        if (message_bytes.size() < 3) {
            continue; // We timed out reading the header.
        }

        Packet::Header header;
        try {
            header = Packet::parseHeader(message_bytes);
        } catch (std::invalid_argument& e) {
            Log::warn(portName(), std::string("Failed to read packet header: ") + e.what());
            // Packet's header was corrupted
            waitDeviceIdle();
            continue;
        }
        if (!request->checkResponseHeader(header)) {
            // The request object rejected the response's header.
            // Before abandoning this try, check if this is an Error packet.
            if (header.type == Packet::Type::Error && header.len == 1) {
                if (readChunked(message_bytes, header.len + 1)) {
                    // We got the full packet
                    try {
                        ErrorResponse err(message_bytes);
                        Log::warn(portName(), std::string("Got a ") + err.getErrorString() + " error");
                        if (  err.getError() == ErrorResponse::Error::CHECKSUM
                           || err.getError() == ErrorResponse::Error::TYPE
                           || err.getError() == ErrorResponse::Error::LENGTH) {
                            // Possibly a one time error, go ahead with a retry
                            std::this_thread::sleep_for(std::chrono::milliseconds(1*T_BYTE));
                            continue;
                        } else {
                            // Device checksummed the packet, so a transmission error is unlikely
                            // Retrying probably won't solve the error, so just fail the query now
                            return nullptr;
                        }
                    } catch (std::exception& e) {
                        // something was wrong with the Error packet. Pessimistically assume that more data was sent as well
                        Log::warn(portName(), std::string("Failed to read error packet: ") + e.what());
                        waitDeviceIdle();
                        continue;
                    }
                } else {
                    // Timed out reading the rest of the packet. Continue directly on to the next try
                    continue;
                }
            } else {
                // Something was corrupted in the reply. Wait for the device to stop talking, then retry
                Log::warn(portName(), "Invalid response header with type " + std::to_string((uint8_t) header.type) + " (expecting " + std::to_string((uint8_t) request->type()) + ") and length " + std::to_string(header.len));
                waitDeviceIdle();
                continue;
            }
        } else {
            // The request approves of the reply's header. Go ahead and read the rest of the reply
            if (readChunked(message_bytes, header.len + 1)) {
                auto response = request->parseResponse(message_bytes);
                if (!response) {
                    // The parse failed. Make sure the device is done talking, then try again
                    // parseResponse will post a log message explaining why it failed
                    waitDeviceIdle();
                    continue;
                } else {
                    // We got a good reply and successfully parsed it
                    return response;
                }
            } else {
                Log::warn(portName(), "Timeout while reading " + std::to_string(header.len + 4) + " byte packet (got " + std::to_string(message_bytes.size()) + " bytes)");
                // Timed out reading the rest of the packet. Continue directly on to the next try
                continue;
            }
        }
    } while (retryCount < RETRY_COUNT);

    // We hit the retry limit. Declare the query a failure
    return nullptr;
}

void Talker::send(std::shared_ptr<Request> request) {
    m_port->flush();
    m_port->write(request->getBytes());
}

bool Talker::readChunked(std::vector<uint8_t>& message_bytes, uint16_t count) {
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

void Talker::waitDeviceIdle() {
    bool finished = false;

    while (!finished) {
        auto data = m_port->read(1, 1 * T_BYTE);
        if (data.size() < 1) finished = true;
    }
}

}} // end namespace cuauv::serial
