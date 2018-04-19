#pragma once

#include <memory>
#include <unordered_set>
#include <mutex>
#include <thread>
#include <string>

#include <Register.h>
#include <Variable.h>
#include <DeviceInfo.h>
#include <Port.h>
#include <packet_types/All.h>

#include "DeviceTalker.h"

namespace cuauv {
namespace serial {
namespace cli {

/**
 * @brief A class which simulates a serial device
 */
class FakeDevice {
    public:
        /**
         * Creates a FakeDevice object on the given port. The constructed object
         * fully simulates a serial device described by the given DeviceInfo object
         *
         * @param portId the identifier of the port for the device to talk on
         * @param info the DeviceInfo representing the device to fake
         */
        FakeDevice(std::string portId, std::shared_ptr<DeviceInfo> info);

        /**
         * Gets a string representing the port the FakeDevice is bound to
         *
         * @returns a string representation of the port
         */
        std::string portName() const;

        /**
         * Starts the communication thread. Has no effect if the thread is already running
         */
        void start();

        /**
         * Stops the communication thread. Has no effect if the thread is already stopped
         */
        void stop();

        /**
         * Returns whether the communication thread is currently running.
         *
         * If an unrecoverable error occurs, the communication thread may unexpectedly exit.
         * This method is the primary way such a condition is reported
         */
        bool isRunning() const;

        /**
         * Returns whether the device is currently connected to a host
         */
        bool isConnected() const;

        /**
         * Sets the value of a Variable that the host can read
         */
        void setReadVar(const BoundVariable& var);
        
        /**
         * Gets the set of variables written by the host
         */
        std::map<std::string, BoundVariable> getWriteVars() const;

        /**
         * Gets the current soft kill state
         */
        bool getSoftKill() const;

        /**
         * Sets the current hard kill state
         */
        void setHardKill(bool hardKill);

        /**
         * Returns whether hard kill is currently set
         *
         * @returns true if hard kill is set
         */
        bool getHardKill() const;

        /**
         * Sets whether the hard kill state is valid
         */
        void setHardKillValid(bool hardKillValid);

        /**
         * Returns whether hard kill is currently valid
         *
         * @returns true if hard kill is valid
         */
        bool getHardKillValid() const;
    private:
        /**
         * Decodes a port identifier string into an actual Port object
         *
         * @param portId the identifier for the port
         *
         * @returns a pointer to a Port object identified by portId
         */
        std::unique_ptr<Port> getPort(std::string portId);

        /**
         * Sets the shutdown flag in the communication thread
         */
        void trigger_shutdown();

        /**
         * Puts the device into the disconnected state
         */
        void doDisconnect();

        /**
         * Processes a received Hello request
         *
         * @request the request to process
         */
        void handleHello(std::shared_ptr<HelloRequest> request);

        /**
         * Processes a received Reset request
         *
         * @request the request to process
         */
        void handleReset(std::shared_ptr<ResetRequest> request);

        /**
         * Processes a received Heartbeat request
         *
         * @request the request to process
         */
        void handleHeartbeat(std::shared_ptr<HeartbeatRequest> request);

        /**
         * Processes a received ReadRange request
         *
         * @request the request to process
         */
        void handleReadRange(std::shared_ptr<ReadRangeRequest> request);

        /**
         * Processes a received WriteRange request
         *
         * @request the request to process
         */
        void handleWriteRange(std::shared_ptr<WriteRangeRequest> request);

        /**
         * Processes a received ReadIndexed request
         *
         * @request the request to process
         */
        void handleReadIndexed(std::shared_ptr<ReadIndexedRequest> request);

        /**
         * Processes a received WriteIndexed request
         *
         * @request the request to process
         */
        void handleWriteIndexed(std::shared_ptr<WriteIndexedRequest> request);

        /**
         * Processes a received Disconnect request
         *
         * @request the request to process
         */
        void handleDisconnect(std::shared_ptr<DisconnectRequest> request);

        /**
         * The main method for the communication thread
         */
        void run();

        /// The communication thread object
        mutable std::thread m_thread;
        /// When true, indicates that the communication thread should exit
        bool m_shouldTerminate;
        /// When true, the communication thread is running. Set to false when the thread exits
        bool m_threadRunning;

        /// The main mutex for interacting with the communication thread
        mutable std::mutex m_mutex;

        BoundRegisterRange m_readRegs;

        std::map<std::string, BoundVariable> m_writeVars;

        bool m_softKill;
        bool m_hardKill;
        bool m_hardKillValid;
        bool m_isConnected;

        std::chrono::time_point<std::chrono::steady_clock> m_lastHeartbeat;
        bool m_gotHello;

        std::shared_ptr<DeviceInfo> m_devInfo;

        DeviceTalker m_talker;
};

}}} // end namespace cuauv::serial::cli
