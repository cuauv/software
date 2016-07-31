#pragma once

#include <memory>

#include <Port.h>
#include <Packet.h>

namespace cuauv {
namespace serial {
namespace cli {

class DeviceTalker {
    public:
        DeviceTalker(std::unique_ptr<Port> port);

        std::string portName() const;

        void send(std::shared_ptr<Response> response);

        std::shared_ptr<Request> listen(unsigned int timeout);

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

        /**
         * Waits for the device to stop sending data. Performs blocking reads until
         * no bytes are read
         */
        void waitHostIdle();

        std::shared_ptr<Request> parseRequest(const std::vector<uint8_t>& request);

        std::unique_ptr<Port> m_port;
};

}}} // end namespace cuauv::serial::cli
