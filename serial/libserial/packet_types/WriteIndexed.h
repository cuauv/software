#pragma once

#include "../Packet.h"
#include "../Register.h"

namespace cuauv {
namespace serial {

/// @brief Request for the WriteIndexed packet type
class WriteIndexedRequest : public Request {
    public:
        /**
         * The main constructor
         *
         * @param regs the set of registers with values to write
         */
        explicit WriteIndexedRequest(const BoundRegisterSet& regs);

        explicit WriteIndexedRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;

        /**
         * Gets the set of registers and values to be written
         *
         * @returns the set of registers and values to be written
         */
        BoundRegisterSet getRegs() const;

    private:
        /// The set of registers and values to be written
        BoundRegisterSet m_regs;
};

/// @brief Response for the WriteIndexed packet type
class WriteIndexedResponse : public Response {
    public:
        /**
         * WriteIndexed response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        WriteIndexedResponse(const WriteIndexedRequest& request, const std::vector<uint8_t>& response);

        WriteIndexedResponse();

        virtual std::vector<uint8_t> getBytes() const override;

    private:
};

}} // end namespace cuauv::serial
