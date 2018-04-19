#pragma once

#include <cstdint>
#include <vector>
#include <memory>

#include "DeviceInfo.h"

// macro to suppress unused variable errors
// http://stackoverflow.com/questions/3599160/unused-parameter-warnings-in-c-code
// Defined here so that it's available to all the packet types
#ifdef __GNUC__
#  define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#  define UNUSED(x) UNUSED_ ## x
#endif

namespace cuauv {
namespace serial {

/// @brief interface class for a serial packet
class Packet {
    public:
        /// The values of a packet's type field
        enum class Type : uint8_t {
            Hello = 0x80,
            Reset = 0x81,
            Heartbeat = 0x82,
            ReadRange = 0x83,
            WriteRange = 0x84,
            ReadIndexed = 0x85,
            WriteIndexed = 0x86,
            Disconnect = 0x87,
            Error = 0x88
        };

        /// A packet header
        typedef struct {
            /// The type of the packet
            Type type;
            /// The length of the packet
            uint16_t len;
        } Header;

        virtual ~Packet() = 0;
        
        /**
         * Gets the packet's representation as a series of bytes
         */
        virtual std::vector<uint8_t> getBytes() const = 0;

        /**
         * Parses a packet's header
         *
         * @param header the packet header. Must be at least three elements long
         */
        static Header parseHeader(const std::vector<uint8_t>& header);

        /**
         * Sets the port used for a packet, for use in log messages
         */
        void setPort(const std::string& port);

        Type type() const;

        bool isResponse() const;

    protected:
        /**
         * Main constructor
         *
         * @param type the packet's type
         * @param isResponse true if the packet is a response packet
         */
        Packet(Type type, bool isResponse);
        /**
         * Initializes a byte representation of a packet. Creates the vector
         * and reserves the full size of the packet (to reduce unnecessary resizes),
         * and inserts the packet's header.
         *
         * @param len the length of the packet's payload
         *
         * @returns a vector for the packet's payload to be added to
         */
        std::vector<uint8_t> startPacket(uint16_t len) const;

        /**
         * Finalizes the packet and inserts the checksum. On return, the vector
         * argument contains the proper byte representation of the packet
         *
         * @param packet the packet to finish, with all payload already added.
         *
         * @throws std::invalid_argument if the header's length field doesn't match
         * the length of the packet's payload
         */
        void finishPacket(std::vector<uint8_t>& packet) const;

        /**
         * Checks that the packet as a whole is valid. Verifies that the length
         * in the header matches the length of the packet, the packet's type matches,
         * and that the checksum is correct
         *
         * @param packet the packet to check
         *
         * @returns true if the packet is valid
         */
        bool checkPacket(const std::vector<uint8_t>& packet) const;

        /**
         * Gets the payload from a packet. Also checks that the packet as a whole
         * is valid (see checkPacket())
         *
         * @throws invalid_argument if the packet is not valid
         *
         * @param packet the packet to retrieve the payload from
         *
         * @returns the packet's payload
         */
        std::vector<uint8_t> getPayload(const std::vector<uint8_t>& packet) const;

    private:
        /// The packet's type
        const Type m_type;

        /// True if the packet is a response type, false if request
        const bool m_isResponse;

        /// A string identifying the port the packet is sent on, for logging purposes
        std::string m_port;
};

/// @brief interface class for the response form of a packet
class Response : public Packet {
    protected:
        /**
         * Main constructor
         *
         * @param type the packet's type
         */
        Response(Type type);

};

/// @brief interface class for the request form of a packet
class Request : public Packet {
    public:
        /**
         * Parses the bytes of a device's response into a Response object. Note that some types
         * of packets have only a request form and not a response form. For those types, this
         * function will always fail
         *
         * @param response the bytes returned by the device as the response
         *
         * @returns the parsed response, or nullptr if parsing failed for any reason
         */
        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const = 0;

        /**
         * Checks that a response's header is valid for the request. A response's header is valid
         * if it is of the same type as the request, and the length is a valid length for a response
         * to the request.
         *
         * @param header the response's header
         *
         * @returns true if the header is valid
         */
        virtual bool checkResponseHeader(const Header& header) const = 0;
        
    protected:
        /**
         * Main constructor
         *
         * @param type the packet's type
         */
        Request(Type type);
};

}} // end namespace cuauv::serial
