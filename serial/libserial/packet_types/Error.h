#pragma once

#include "../Packet.h"

namespace cuauv {
namespace serial {

/// @brief Response for the Error packet type
class ErrorResponse : public Response {
    public:
        /// The error codes which the device can return
        enum class Error : uint8_t {
            TYPE = 0x00,
            CHECKSUM = 0x01,
            RANGE = 0x02,
            LENGTH = 0x03
        };

        /**
         * Error response parsing constructor
         *
         * @param response the device's error response
         */
        explicit ErrorResponse(const std::vector<uint8_t>& response);

        /**
         * Error response construtor
         *
         * @param error the error reported in this response
         */
        explicit ErrorResponse(Error error);

        virtual std::vector<uint8_t> getBytes() const override;

        /**
         * Returns the error code sent by the device
         *
         * @returns the error code
         */
        Error getError() const;

        /**
         * Returns a string describing the error code sent by the device
         *
         * @returns the stringified error code
         */
        std::string getErrorString() const;

    private:
        /// The error code sent by the device
        Error m_error;
};

}} // end namespace cuauv::serial
