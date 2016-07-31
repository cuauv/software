/*!
 * \file device.h
 * \author Peter Tseng
 */

#ifndef _USD_DEVICE_H_
#define _USD_DEVICE_H_

#include <functional>
#include <queue>
#include <vector>
#include <pthread.h>
#include <stdint.h>
#include <sys/time.h>

namespace sensorserial {
    class SerialPort;
}

class PollBlock;

/*!
 * \class Device
 * \brief A device conforming to the serial protocol.
 *
 * The Device class represents a physical device, encapsulating communication
 * on the serial port and interfacing with shared memory.
 */
class Device {
public:
    Device(int id, const char* name, const char* devPath, int baudRate, bool debugSend,
        bool debugReceive);
    virtual ~Device() { delete[] devPath; delete[] name; }
    void addPollBlock(PollBlock* block);
    bool setPath(const char* path);
    void start();
    void stop();
    void makeReadRequest(uint8_t count, uint16_t address);
    void makeWriteRequest(uint8_t count, uint16_t address, uint8_t* payload);
    const char* getName();

    void dump();

    int join();

    void mainLoop();
    void readLoop();
    void watchLoop();

protected:
    bool isActive() const;
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) = 0;
    virtual void watch() = 0;

    static int int16FromBuffer(uint8_t* buffer);
    static int int32FromBuffer(uint8_t* buffer);
    static double floatFromBuffer(uint8_t* buffer);
    static bool boolFromBuffer(uint8_t* buffer);

    void sendInt16(uint16_t address, int x);
    void sendInt32(uint16_t address, int x);
    void sendFloat(uint16_t address, double x);
    void sendBool(uint16_t address, bool x);

private:
    /*!
     * \brief Microseconds pause between each message: 22 ms for now.
     * Explanation: current serial code on hardware side claims to require 15 ms
     * between each packet. Experimentally, we have determined that anything
     * faster than ~21 ms causes problems (packets "run into" each other). We
     * shall use 22 ms to be safe. So we can still run at about 45Hz if we like.
     * If we want something faster, the hardware guys need to change their
     * implementation.
     */
    static const int WRITE_INTERVAL = 22000;

    //! Timeval for the message gap.
    static timeval writeIntervalTime;

    //! ID of the device.
    const int id;

    //! Name of the device
    char* name;
    bool setName(const char* name);

    //! Path (typically in /dev) to the serial port.
    char* devPath;
    //! Baud rate for the serial port.
    const int baudRate;
    //! Serial port this Device will use for communications.
    sensorserial::SerialPort* port;

    //! Whether to debug packets being sent to the physical device.
    const bool debugSend;
    //! Whether to debug packets received from the physical device.
    const bool debugReceive;

    //! Whether this Device has been started (and the threads are running).
    bool active;

    //! Opaque identifier for this device's main thread.
    pthread_t mainThread;
    //! Opaque identifier for this device's read thread.
    pthread_t readThread;
    //! Opaque identifier for this device's watch thread.
    pthread_t watchThread;
    //! Key for thread-specific buffers for storing packet data.
    pthread_key_t bufferKey;
    //! Mutex for next available write time.
    pthread_mutex_t nextWriteMutex;

    /*!
     * \brief When it is next okay to write to serial. Needed because the code
     * on the device side needs a 15ms pause between messages.
     */
    timeval nextWrite;

    /*!
     * \struct ComparePollBlockPointers
     * \brief Comparison class comparing two PollBlock pointers.
     *
     * Compares two PollBlock pointers by dereferencing the pointers and
     * comparing the timevals of their next update. The PollBlock with the
     * smaller (sooner) update time gets higher priority.
     */
    struct ComparePollBlockPointers :
            public std::binary_function<PollBlock*, PollBlock*, bool> {
        bool operator()(const PollBlock* lhs, const PollBlock* rhs) const;
    };

    //! Priority queue of PollBlock to poll, keyed by next poll time.
    std::priority_queue<PollBlock*, std::vector<PollBlock*>,
        ComparePollBlockPointers> pollSchedule;

    void writeMessage(int size, uint8_t* message);

    static bool isValidResponse(uint8_t function);

    static void* runMainLoop(void* device);
    static void* runReadLoop(void* device);
    static void* runWatchLoop(void* device);

};

#endif
