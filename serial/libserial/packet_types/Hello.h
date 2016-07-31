#pragma once

#include "../Packet.h"

namespace cuauv {
namespace serial {

/// @brief Request for the Hello packet type
class HelloRequest : public Request {
    public:
        /**
         * The main constructor
         */
        HelloRequest();

        explicit HelloRequest(const std::vector<uint8_t>& request);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;
};

/// @brief Response for the Hello packet type
class HelloResponse : public Response {
    public:
        /**
         * Hello response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        HelloResponse(const HelloRequest& request, const std::vector<uint8_t>& response);

        explicit HelloResponse(std::shared_ptr<DeviceInfo> devInfo);

        virtual std::vector<uint8_t> getBytes() const override;

        /**
         * Returns the protocol version field from the Hello response packet
         *
         * @return the protocol version
         */
        uint8_t getProtocolVersion() const;

        /**
         * Returns the reset flags field from the Hello response packet
         *
         * @return the reset flags
         */
        uint8_t getResetFlags() const;

        /**
         * Returns the DeviceInfo object generated from the Hello response packet
         *
         * @return the DeviceInfo for this device
         */
        std::shared_ptr<DeviceInfo> getDeviceInfo() const;

    private:
        /// The device info generated from this response packet
        std::shared_ptr<DeviceInfo> m_devInfo;
};

}} // end namespace cuauv::serial
