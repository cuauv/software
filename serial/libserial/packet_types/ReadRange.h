#pragma once

#include "../Packet.h"
#include "../Register.h"

namespace cuauv {
namespace serial {

/// @brief Request for the ReadRange packet type
class ReadRangeRequest : public Request {
    public:
        /**
         * The main constructor
         *
         * @param range the range to read
         */
        explicit ReadRangeRequest(const RegisterRange& range);

        explicit ReadRangeRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;

        /**
         * Gets the range of registers to be read
         *
         * @returns the range of registers to be read
         */
        RegisterRange getRange() const;

    private:
        /// The range of registers
        RegisterRange m_range;
};

/// @brief Response for the ReadRange packet type
class ReadRangeResponse : public Response {
    public:
        /**
         * ReadRange response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        ReadRangeResponse(const ReadRangeRequest& request, const std::vector<uint8_t>& response);

        explicit ReadRangeResponse(const BoundRegisterRange& range);

        virtual std::vector<uint8_t> getBytes() const override;

        /**
         * Gets the range of registers read, with values
         *
         * @returns the range of registers read, with values
         */
        BoundRegisterRange getRange() const;

    private:
        /// The range of registers with values
        const BoundRegisterRange m_range;
};

}} // end namespace cuauv::serial
