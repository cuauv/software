#pragma once

#include <memory>
#include <string>

#include "Port.h"
#include "Packet.h"

namespace cuauv {
namespace serial {

/**
 * @brief Handles sending and receiving messages over serial
 *
 * A Talker object wraps a Port and provides methods for sending and receiving
 * entire Request and Response objects. Talker handles all serialization and
 * deserialization, and implements all per-message timeout behavior as well.
 * Note that this means calls to Talker could block for long periods of time,
 * waiting for a device's response.
 */
class Talker {
    public:
        /**
         * Creates a serial talker and binds it to a port
         *
         * @param port the port to communicate over
         */
        Talker(std::unique_ptr<Port> port);

        /**
         * Returns a string representing the port's name
         *
         * @returns a friendly string representation of the port
         */
        std::string portName() const;

        /**
         * Performs a request-response query, where the talker sends
         * a request to the device, and expects a response in return. Retries are
         * attempted in the case of timeouts or detected transmission errors
         *
         * On return from this function, the device should always be in an idle state
         * (ie it is not transmitting any data, and it does not think a packet is in progress).
         * This rule means that under certain error conditions, this function will block
         * for a long period of time, waiting for the device to stop sending unwanted data.
         *
         * @throws std::runtime_error for an unrecoverable communications failure.
         * If such an exception is thrown, assume that no further attempts to communicate
         * will succeed.
         * 
         * @param request the request to transmit to the device
         *
         * @returns the device's response, or nullptr if no response
         * was received
         */
        std::shared_ptr<Response> query(std::shared_ptr<Request> request);

        /**
         * Sends a request to a device, without waiting for a reply. Blocks until
         * all bytes have been transmitted to the device.
         *
         * @throws std::runtime_error for an unrecoverable communications failure.
         * If such an exception is thrown, assume that no further attempts to communicate
         * will succeed.
         * 
         * @param request the request to transmit to the device
         */
        void send(std::shared_ptr<Request> request);

        /**
         * Waits for the device to stop sending data. Performs blocking reads until
         * no bytes are read
         */
        void waitDeviceIdle();

    private:
        /**
         * Performs a chunked read of len bytes from the port
         *
         * @param message_bytes the vector to push the data into
         * @param count the amount of data to read
         *
         * @returns true if the read succeeded, false if it timed out
         */
        bool readChunked(std::vector<uint8_t>& message_bytes, uint16_t count);

        /// The port to talk over
        std::unique_ptr<Port> m_port;
};

}} // end namespace cuauv::serial
