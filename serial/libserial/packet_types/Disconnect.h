#pragma once

#include "../Packet.h"

namespace cuauv {
namespace serial {

/// @brief Request for the Disconnect packet type
class DisconnectRequest : public Request {
    public:
        /**
         * The main constructor
         */
        DisconnectRequest();

        explicit DisconnectRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;
};

}} // end namespace cuauv::serial
