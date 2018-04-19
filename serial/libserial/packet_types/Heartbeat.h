#pragma once

#include "../Packet.h"

namespace cuauv {
namespace serial {

/// @brief Request for the Heartbeat packet type
class HeartbeatRequest : public Request {
    public:
        /**
         * The main constructor
         */
        explicit HeartbeatRequest(bool softKill);

        explicit HeartbeatRequest(const std::vector<uint8_t>& packet);

        virtual std::vector<uint8_t> getBytes() const override;

        virtual bool checkResponseHeader(const Header& header) const override;

        virtual std::shared_ptr<Response> parseResponse(const std::vector<uint8_t>& response) const override;

        /**
         * Returns the soft kill flag in this request
         *
         * @returns true if soft kill is set
         */
        bool getSoftKill() const;

    private:
        /// The soft kill state
        bool m_softKill;
};

/// @brief Response for the Heartbeat packet type
class HeartbeatResponse : public Response {
    public:
        /**
         * Heartbeat response parsing constructor
         *
         * @param request the request which generated this response
         * @param response the device's response
         */
        HeartbeatResponse(const HeartbeatRequest& request, const std::vector<uint8_t>& response);

        HeartbeatResponse(bool hardKill, bool hardKillValid, uint16_t heartbeatTime);

        virtual std::vector<uint8_t> getBytes() const override;

        /**
         * Returns the hard kill value from the Heartbeat response
         * 
         * Only meaningful if isHardKillValid() returns true
         *
         * @returns true if hard killed
         */
        bool getHardKill() const;

        /**
         * Returns whether hard kill is valid for this Heartbeat response
         *
         * @returns true if hard kill has a valid value
         */
        bool isHardKillValid() const;

        /**
         * Returns the time in ms since the last Heartbeat packet
         *
         * @returns the time in ms since the last Heartbeat packet
         */
        uint16_t getHeartbeatTime() const;

    private:
        /// The hard kill state
        bool m_hardKill;

        /// Whether the hard kill state is valid
        bool m_hardKillValid;

        /// The time since the last heartbeat packet
        uint16_t m_heartbeatTime;
};

}} // end namespace cuauv::serial
