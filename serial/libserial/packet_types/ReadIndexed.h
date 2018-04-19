#pragma once

#include "../Packet.h"
#include "../Register.h"

namespace cuauv {
namespace serial {

/// @brief Request for the ReadIndexed packet type
class ReadIndexedRequest : public Request {
    public:
        /**
         * The main constructor
         *
         * @param regs the set of registers to read
         */
        explicit ReadIndexedRequest(const RegisterSet& regs);

        explicit ReadIndexedRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;

        /**
         * Gets the set of registers to be read
         *
         * @returns the set of registers to be read
         */
        RegisterSet getRegs() const;

        /**
         * Gets the order of the registers passed in this request
         *
         * @returns a vector, giving the in-order list of registers
         */
        std::vector<uint8_t> getRegOrder() const;
    private:
        /// The set of registers to be read
        const RegisterSet m_regs;
        std::vector<uint8_t> m_regOrder;
};

/// @brief Response for the ReadIndexed packet type
class ReadIndexedResponse : public Response {
    public:
        /**
         * ReadIndexed response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        ReadIndexedResponse(const ReadIndexedRequest& request, const std::vector<uint8_t>& response);

        ReadIndexedResponse(const BoundRegisterSet& regs, const std::vector<uint8_t>& regOrder);

        virtual std::vector<uint8_t> getBytes() const override;

        /**
         * Gets the registers and values which were read
         *
         * @returns the set of registers and values which were read
         */
        BoundRegisterSet getRegs() const;

        /**
         * Gets the order of the registers passed in this response
         *
         * @returns a vector, giving the in-order list of registers
         */
        std::vector<uint8_t> getRegOrder() const;
    private:
        /// The set of registers and values which were read
        const BoundRegisterSet m_regs;

        const std::vector<uint8_t> m_regOrder;
};

}} // end namespace cuauv::serial
