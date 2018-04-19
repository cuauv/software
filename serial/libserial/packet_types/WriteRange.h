#pragma once

#include "../Packet.h"
#include "../Register.h"

namespace cuauv {
namespace serial {

/// @brief Request for the WriteRange packet type
class WriteRangeRequest : public Request {
    public:
        /**
         * The main constructor
         *
         * @param range the bound range of registers to write
         */
        explicit WriteRangeRequest(const BoundRegisterRange& range);

        explicit WriteRangeRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;

        /**
         * Gets the bound range of registers to be written
         *
         * @returns the bound range of registers to be written
         */
        BoundRegisterRange getRange() const;
    private:
        /// The bound range of registers to be written
        BoundRegisterRange m_range;
};

/// @brief Response for the WriteRange packet type
class WriteRangeResponse : public Response {
    public:
        /**
         * WriteRange response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        WriteRangeResponse(const WriteRangeRequest& request, const std::vector<uint8_t>& response);

        WriteRangeResponse();

        virtual std::vector<uint8_t> getBytes() const override;

    private:
};

}} // end namespace cuauv::serial
