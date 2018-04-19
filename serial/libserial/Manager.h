#pragma once

#include <string>
#include <functional>
#include <memory>
#include <unordered_set>
#include <set>
#include <map>
#include <vector>
#include <chrono>
#include <string>

#include <thread>
#include <mutex>
#include <condition_variable>

#include "DeviceCallbacks.h"
#include "DeviceInfo.h"
#include "Variable.h"
#include "Port.h"
#include "Talker.h"

namespace cuauv {
namespace serial {

/**
 * @brief The main interface class for libserial
 *
 * Each Manager object controls the entire operation of a single serial port.
 * When a device is detected, the onConnected callback passed into the constructor is called
 * with information about the connected device. This function returns an object containing the
 * rest of the callbacks used during the lifecycle of a connected device.
 */
class Manager {
    public:
        /**
         * A callback for handling a newly connected device.
         *
         * Arguments are a pointer to a Manager object, and a DeviceInfo object.
         * Return value is a smart pointer to a DeviceCallbacks object, or nullptr if the connection should be aborted.
         */
        typedef std::function<std::shared_ptr<DeviceCallbacks>(Manager*, std::shared_ptr<DeviceInfo>)> connect_callback_t;

        /**
         * A callback for when an unrecoverable failure occurs with the port
         *
         * Arguments are a pointer to the failed Manager object, and a string describing the error.
         *
         * In general, the only corrective action that can be taken is to destroy and re-instantiate the Manager. However,
         * this callback occurs on the communication thread, so destroying the Manager is not permitted. Any action which results
         * in the Manager's destructor running on the communication thread will most likely result in program termination, at best.
         *
         * Proper procedure for handling this error is to signal for another thread to shut down the Manager, and optionally for that
         * thread to create a new one (it will have to probe for ports again, however)
         */
        typedef std::function<void(Manager*, std::string)> failure_callback_t;


        /**
         * Initializes a Manager for a specific port, identified by a string.
         *
         * The manager is initially created in the idle state. A call to start() must be made before the manager
         * will make any attempt to communicate with the device
         *
         * @param port the port to communicate on. A list of available ports may be retrieved by calling listPorts()
         *
         * @param onConnected a callable which takes information about a newly connected device and returns an object
         * with various callback functions. The callback is permitted to schedule initial reads and writes by calling
         * submitRead() and submitWrite() on the manager object passed into it. If the callback returns nullptr, then
         * the connection is aborted, and any scheduled reads or writes are ignored. The port will continue scanning
         * for a device if such an event occurs.
         *
         * @param onPortFailed a callable for handling an unrecoverable failure on the port. See the comments for
         * failure_callback_t for details on the restrictions surrounding this callback.
         */
        Manager(std::string port, connect_callback_t onConnected, failure_callback_t onPortFailed);

        /**
         * Initializes a Manager for a given port object. This is intended primarily for test code; users of this library
         * are encouraged to use the string-based constructor instead.
         *
         * The manager is initially created in the idle state. A call to start() must be made before the manager
         * will make any attempt to communicate with the device
         *
         * @param port the port object to communicate on.
         *
         * @param onConnected a callable which takes information about a newly connected device and returns an object
         * with various callback functions. The callback is permitted to schedule initial reads and writes by calling
         * submitRead() and submitWrite() on the manager object passed into it. If the callback returns nullptr, then
         * the connection is aborted, and any scheduled reads or writes are ignored. The port will continue scanning
         * for a device if such an event occurs.
         *
         * @param onPortFailed a callable for handling an unrecoverable failure on the port. See the comments for
         * failure_callback_t for details on the restrictions surrounding this callback.
         */
        Manager(std::unique_ptr<Port> port, connect_callback_t onConnected, failure_callback_t onPortFailed);

        /**
         * Destroys the Manager object, terminating any active communication
         *
         * Implicitly calls stop() as well, so the destructor may block on any pending callbacks
         */
        ~Manager();

        /**
         * Gets a friendly name representing the port this Manager is bound to
         *
         * @returns a string representing the port in use
         */
        std::string portName() const;

        /**
         * Starts communicating on the serial port, or resumes if a call to disconnect() or resetDevice() was made.
         * 
         * Has no effect if the manager is already communicating
         */
        void start();

        /**
         * Immediately stops communication on the port. Does not send any disconnect messages on the
         * port, and does not call DeviceCallbacks::onDisconnected(). This function blocks until any
         * in-flight callbacks have completed and all communication has stopped
         */
        void stop();

        /**
         * Returns whether the manager is currently communicating on the port
         *
         * @returns true if the manager is currently communicating
         */
        bool isRunning() const;

        /**
         * Returns whether a device is currently connected
         *
         * @return true if a device is currently connected
         */
        bool isConnected() const;

        /**
         * Performs a clean disconnect of a device which is connected (if any), and
         * stops all communication on the serial port. This function blocks until
         * any in-flight callbacks have completed and the device is fully disconnected.
         *
         * The port may be later reactivated with a call to start(). Note that 
         * DeviceCallbacks::onDisconnected() will be called from a different thread while handling the disconnect.
         */
        void disconnect();

        /**
         * Commands a device to perform a remote reset, and disconnects from the serial port
         * If there is no device currently connected, then the behavior is identical to that
         * of disconnect(). Blocks until any callbacks have completed and the device is fully
         * disconnected.
         *
         * The port may be later reactivated with a call to start(). Note that 
         * DeviceCallbacks::onDisconnected() will be called from a different thread while handling the disconnect.
         */
        void resetDevice();

        /**
         * Submits a read operation to the connected device. If no device is connected, this function
         * is effectively a no-op. If a device is in the process of connecting, the read will occur once the device
         * finishes connecting. DeviceCallbacks::onRead() will be called when the result is ready.
         *
         * @param vars the set of variables to be read
         */
        void submitRead(const std::unordered_set<Variable>& vars);

        /**
         * Submits a write operation to the connected device. If no device is connected, this function
         * is effectively a no-op. If a device is in the process of connecting, the write will occur once the device
         * finishes connecting. DeviceCallbacks::onWrite() will be called when the write completes.
         *
         * @param vars the set of variables to be written
         */
        void submitWrite(const std::unordered_set<BoundVariable>& vars);

        /**
         * Returns the list of available serial ports
         *
         * Note that the existence of a port on this list does not guarantee that
         * a Manager can be constructed on it. The port may already be in use by another process,
         * or the port may disappear between the call to listPorts() and the call to the constructor.
         */
        static std::set<std::string> listPorts();

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
         * Handles cleanup when a device disconnects
         */
        void deviceDisconnected();

        /**
         * Sends a heartbeat to the device, if necessary
         */
        void processHeartbeat();

        /**
         * Handles any reads which are currently pending for the device
         */
        void processPendingReads();

        /**
         * Handles any writes which are currently pending for the device
         */
        void processPendingWrites();

        /**
         * Sends a Hello packet to check for a device, and sleeps for a little
         * while if no reply is received
         */
        void pollForDevice();

        /**
         * Blocks until there's more work for the communication thread
         */
        void waitForNextEvent() const;

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
        /// For signaling when new work is available
        mutable std::condition_variable m_cond;

        /// The queue of manually triggered reads.
        //  Implemented as a vector because of its expected usage pattern.
        //  Clients will push elements into the queue periodically, while the communication
        //  thread will remove all elements at once when ready. Vectors should perform
        //  best under these circumstances
        std::vector<std::unordered_set<Variable>> m_readQueue;
        /// The queue of manually triggered writes.
        //  See m_readQueue for explanation of container type
        std::vector<std::unordered_set<BoundVariable>> m_writeQueue;

        /// The type for time points when used with scheduling
        typedef std::chrono::time_point<std::chrono::steady_clock> time_point_t;

        /// The queue of periodic reads, implemented as a map. The key is the time that the next
        //  read should occur, while the value is the details about the read.
        std::multimap<time_point_t, std::pair<int, std::unordered_set<Variable>>> m_periodicReadQueue;

        /// The time when the next heartbeat should occur
        time_point_t m_nextHeartbeat;

        /// The talker handling packet transmission and reception on the port
        Talker m_talker;

        /// The callback function for handling initial device connection
        const connect_callback_t m_onConnect;

        /// The callback function for handling an unrecoverable port failure
        const failure_callback_t m_onPortFailure;

        /// The various callbacks provided by the client code
        //  Only non-null when a device is connected, the manager is in a
        //  disconnected state if this pointer is null
        std::shared_ptr<DeviceCallbacks> m_devCallbacks;
};

}} // end namespace cuauv::serial
