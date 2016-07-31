#include <cstring>
#include <iostream>
#include <netinet/in.h>
#include <signal.h>

#include "device.h"
#include "pollblock.h"
#include "protocol.h"

#include "../sensors/serial/serial.h"

using namespace sensorserial;
using namespace std;

timeval Device::writeIntervalTime;

/*!
 * \brief Constructs a device with the specified path, baud rate, and variables.
 *
 * \param id ID of the device.
 * \param path Path (Typically in /dev) to the serial port.
 * \param baudRate Baud rate for the serial port.
 * \param debugSend Whether to debug packets sent to this device.
 * \param debugReceive Whether to debug packets received from this device.
 */
Device::Device(int id, const char* name, const char* path, int baudRate, bool debugSend,
        bool debugReceive)
        : id(id), baudRate(baudRate), port(NULL),
        debugSend(debugSend), debugReceive(debugReceive),
        active(false) {
    setPath(path);
    setName(name);

    // Create key for thread-specific buffers.
    pthread_key_create(&bufferKey, NULL);
    // Create mutex
    pthread_mutex_init(&nextWriteMutex, NULL);

    // !!! TODO: These should only be run ONCE, since wIT is a class var!
    writeIntervalTime.tv_sec = 0;
    writeIntervalTime.tv_usec = WRITE_INTERVAL;

    nextWrite.tv_sec = 0;
    nextWrite.tv_usec = 0;
}

/*!
 * \brief Adds a PollBlock to this Device.
 *
 * The PollBlock is added to the poll schedule if it is auto-polling, and all
 * writable variables belonging to the PollBlock are added to the Var Watcher.
 *
 * This should only be called after all variables are added to the PollBlock!
 *
 * Will warn if the PollBlock's parent is not this Device. (Shouldn't happen...)
 *
 * \param block PollBlock that this Device should monitor.
 */
void Device::addPollBlock(PollBlock* block) {
    if (block == NULL) {
        cerr << "Device " << id << ": Null poll block!" << endl;
        return;
    }

    // Initialize the poll schedule
    if (block->isAutoPoll()) {
        pollSchedule.push(block);
    }
}

/*!
 * \brief Sets the path of this Device.
 *
 * Ineffective if device is already started.
 *
 * \param path New path of this Device.
 * \return True iff the path was successfully set.
 */
bool Device::setPath(const char* path) {
    if (port == NULL) {
        devPath = new char[strlen(path) + 1];
        strcpy(devPath, path);
        return true;
    }
    return false;
}

bool Device::setName(const char* name) {
    this->name = new char[strlen(name) + 1];
    strcpy(this->name, name);
    return true;
}

/*!
 * \brief Returns the name of this Device.
 * \return char* name
 */
const char* Device::getName() {
    return (const char*) name;
}

/*!
 * \brief Starts the Device.
 *
 * Opens the serial port for communications and starts the three threads.
 */
void Device::start() {
    port = new SerialPort(devPath, baudRate);

    // Start the three threads - main, read, watch
    active = true;
    pthread_create(&mainThread, NULL, runMainLoop, this);
    pthread_create(&readThread, NULL, runReadLoop, this);
    pthread_create(&watchThread, NULL, runWatchLoop, this);
}

/*!
 * \brief Stops the Device.
 *
 * Closes and frees the SerialPort, and stops the three threads.
 */
void Device::stop() {
    // This will cause the threads to exit.
    if (active) {
        active = false;

        // Sleep to give the threads time to finish up.
        sleep(1);

        // I got impatient, let's just forcibly kill the threads.
        pthread_kill(mainThread, SIGKILL);
        pthread_kill(readThread, SIGKILL);
        pthread_kill(watchThread, SIGKILL);
    }

    delete port;
    port = NULL;
}

/*!
 * \brief Makes a read request to be sent to the SerialPort.
 *
 * This function forms the packet header and no payload, passing it off to
 * writeMessage. Currently called by PollBlock::poll() and by the read thread on
 * a read response with a bad checksum.
 *
 * \param count Number of registers to read.
 * \param address Address of the first register to read.
 */
void Device::makeReadRequest(uint8_t count, uint16_t address) {
    uint8_t* message = static_cast<uint8_t*>(pthread_getspecific(bufferKey));

    message[0] = PACKET_TYPE_READ;
    message[1] = count;
    // Be awesome and assign 2 bytes at once!
    *(reinterpret_cast<uint16_t*>(message + 2)) = htons(address);

    writeMessage(BASE_PACKET_LENGTH, message);
}

/*!
 * \brief Makes a write request to be sent to the SerialPort.
 *
 * This function forms the packet header with the specified payload, passing it
 * off to writeMessage. Currently called by SyncedVar::updateRemote().
 *
 * \param count Number of registers to write to.
 * \param address Address of the first register to write to.
 * \param payload Payload to write.
 */
void Device::makeWriteRequest(uint8_t count, uint16_t address, uint8_t* payload) {
    uint8_t* message = static_cast<uint8_t*>(pthread_getspecific(bufferKey));

    message[0] = PACKET_TYPE_WRITE;
    message[1] = count;
    // Be awesome and assign 2 bytes at once!
    *(reinterpret_cast<uint16_t*>(message + 2)) = htons(address);

    // Copy payload into the rest.
    if (count > 0) {
        memcpy(message + BASE_PACKET_LENGTH, payload, 2 * count);
    }

    writeMessage(BASE_PACKET_LENGTH + 2 * count, message);
}

/*!
 * \brief Dumps the variables of this device.
 *
 * Values of all variables are printed to standard out.
 */
void Device::dump() {
    cout << "Not implemented" << endl;
}

/*!
 * \brief Waits to join with the main thread of this Device.
 */
int Device::join() {
    if (active)
        return pthread_join(mainThread, NULL);
    else
        return 0;
}

/*!
 * \brief Loop for the main/control thread.
 *
 * Uses a priority queue to determine the next PollBlock to be polled and calls
 * Pollblock::poll() when needed. Sleeps when no blocks are up for polling.
 */
void Device::mainLoop() {
    // Main thread will only ever call poll(), which will only make read
    // requests - therefore, I can use a smaller buffer.
    uint8_t buffer[BASE_PACKET_LENGTH + 1];
    pthread_setspecific(bufferKey, buffer);

    // Initial ID check.
    makeReadRequest(ID_LENGTH, ID_ADDRESS);

    // If nothing in poll schedule, main thread doesn't need to do anything.
    if (pollSchedule.size() == 0) {
        pthread_setspecific(bufferKey, NULL);
        pthread_join(watchThread, NULL);
        return;
    }

    PollBlock* first = pollSchedule.top();
    timeval firstTime = first->getNextPollTime();
    timeval now;

    while (active) {
        gettimeofday(&now, NULL);

        // While there are still variables to be polled:
        while (timercmp(&firstTime, &now, <=)) {
            // Remove the first var from the queue and poll it.
            pollSchedule.pop();
            makeReadRequest(first->getLength(), first->getStartAddress());
            first->poll();

            // poll() should have updated first.nextPoll. Reinsert in queue.
            pollSchedule.push(first);

            // Update first var and its time so the loop can proceed.
            first = pollSchedule.top();
            firstTime = first->getNextPollTime();
        }

        // At this point, first->getNextPollTime() > NOW
        // No more variables to update, for now.
        // Sleep until first is ready to be updated.
        timeval sleepTime;
        timersub(&firstTime, &now, &sleepTime);
        usleep(sleepTime.tv_sec * 1000000 + sleepTime.tv_usec);
    }

    pthread_setspecific(bufferKey, NULL);
}

/*!
 * \brief Loop for the read thread.
 *
 * Handles all incoming packets from serial, and takes the appropriate action in
 * response. In particular, a read response will call PollBlock::handlePacket.
 */
void Device::readLoop() {
    uint8_t buffer[MAX_PACKET_LENGTH];
    pthread_setspecific(bufferKey, buffer);

    while (active) {
        // Read only the packet type.
        port->readSer(buffer, 1);
        unsigned int badFunctionCount = 0;

        // Hm... this isn't valid. Dump it and try the next one.
        while (!isValidResponse(buffer[0])) {
            ++badFunctionCount;
            if (debugReceive) {
                cout << "Device " << id << ": Dump " << (int) buffer[0] << endl;
            }
            port->readSer(buffer, 1);
        }

        if (debugReceive && badFunctionCount > 0) {
            cout << "Device " << id << ": Dumped " << badFunctionCount
                << " bytes total" << endl;
        }

        // Ah, here's a good packet. Read the rest.
        port->readSer(buffer + 1, BASE_PACKET_LENGTH - 1);

        // For clarity:
        uint8_t packetType = buffer[0];
        uint8_t count = buffer[1];
        uint16_t address = ntohs(*(reinterpret_cast<uint16_t*>(buffer + 2)));

        int size = BASE_PACKET_LENGTH;

        // Of all the response types, only Read Response has a payload!
        if (packetType == (PACKET_TYPE_READ | PACKET_TYPE_RESPONSE)) {
            // Read said payload into buffer:
            port->readSer(buffer + size, 2 * count);
            size += 2 * count;
        }

        // Read checksum and append to end of buffer:
        port->readSer(buffer + size, 1);

        if (debugReceive) {
            cout << "Device " << id << ": Receive ";
            for (int i = 0; i <= size; ++i) {
                cout << (int) buffer[i];
                if (i == size) {
                    cout << endl;
                }
                else {
                    cout << ", ";
                }
            }
        }

        uint8_t checksumReceived = buffer[size];
        uint8_t checksumExpected = computeChecksum(size, buffer);

        if (checksumReceived != checksumExpected) {
            cerr << "Device " << id << ": Read Response for address "
                << address << " with a bad checksum. Was "
                << (int) checksumReceived << ", expected "
                << (int) checksumExpected << endl;
            if (packetType == (PACKET_TYPE_READ | PACKET_TYPE_RESPONSE)) {
                makeReadRequest(count, address);
            }
            continue;
        }

        switch (packetType) {
        // Read Response:
        case (PACKET_TYPE_READ | PACKET_TYPE_RESPONSE):
            // ID register? ID check.
            if (address == ID_ADDRESS) {
                uint16_t receivedId = ntohs(*(reinterpret_cast<uint16_t*>(buffer + BASE_PACKET_LENGTH)));
                if (receivedId != id) {
                    cerr << "Device " << id << ": ID should be " << id
                        << " but reports ID of " << receivedId << endl;
                }
            }
            handleReadResponse(address, count, buffer + BASE_PACKET_LENGTH);
            break;
        case PACKET_TYPE_BAD_LENGTH:
        case PACKET_TYPE_BAD_CHECKSUM:
        case PACKET_TYPE_UNKNOWN_FAILURE:
            // TODO
            break;
        case PACKET_TYPE_INVALID_REGISTER:
            cerr << "Device " << id << ": Invalid Register error for address "
                << address << endl;
            // exit(1);
            break;
        case PACKET_TYPE_READONLY_REGISTER:
            cerr << "Device " << id << ": Read Only Register error for address "
                << address << endl;
            // exit(1);
            break;
        case PACKET_TYPE_INVALID_FUNCTION:
            cerr << "Device " << id << ": Invalid Function error for address "
                << address << endl;
            // exit(1);
            break;
        case (PACKET_TYPE_WRITE | PACKET_TYPE_RESPONSE):
        case (PACKET_TYPE_AND | PACKET_TYPE_RESPONSE):
        case (PACKET_TYPE_OR | PACKET_TYPE_RESPONSE):
        case (PACKET_TYPE_XOR | PACKET_TYPE_RESPONSE):
            // These four do not require any action on our part.
            break;
        default:
            // This shouldn't ever happen anymore...
            cerr << "Device " << id << ": Unknown packet type "
                << (int) packetType << " from device" << endl;
            break;
        }
    }

    pthread_setspecific(bufferKey, NULL);
}

/*!
 * \brief Loop for the watch thread.
 *
 * Uses an AuvVarWatch to watch for changes in shared memory. When there is a
 * change, uses SyncedVar::updateRemote() to push the change to the device.
 */
void Device::watchLoop() {
    uint8_t buffer[MAX_PACKET_LENGTH];
    pthread_setspecific(bufferKey, buffer);

    watch();

    pthread_setspecific(bufferKey, NULL);
}

bool Device::isActive() const {
    return active;
}

int Device::int16FromBuffer(uint8_t* buffer) {
    uint16_t newValue = ntohs(*(reinterpret_cast<uint16_t*>(buffer)));
    return newValue;
}

int Device::int32FromBuffer(uint8_t* buffer) {
    uint32_t newValue = ntohl(*(reinterpret_cast<uint32_t*>(buffer)));
    return newValue;
}

double Device::floatFromBuffer(uint8_t* buffer) {
    union {
        float f;
        uint32_t i;
    } fi;
    memcpy(&fi.i, buffer, sizeof(float));
    fi.i = ntohl(fi.i);
    return fi.f;
}

bool Device::boolFromBuffer(uint8_t* buffer) {
    uint16_t temp = ntohs(*(reinterpret_cast<uint16_t*>(buffer)));
    return temp != 0;
}

void Device::sendInt16(uint16_t address, int x) {
    union {
        uint8_t buffer[sizeof(uint16_t)];
        uint16_t ival;
    };
    ival = htons(x);
    makeWriteRequest(1, address, buffer);
}

void Device::sendInt32(uint16_t address, int x) {
    union {
        uint8_t buffer[sizeof(uint32_t)];
        uint32_t ival;
    };
    ival = htonl(x);
    makeWriteRequest(2, address, buffer);
}

void Device::sendFloat(uint16_t address, double x) {
    union {
        float f;
        uint32_t i;
    } fi;
    uint8_t buffer[sizeof(uint32_t)];
    fi.f = x;
    fi.i = htonl(fi.i);
    memcpy(buffer, &fi.i, sizeof(uint32_t));
    makeWriteRequest(2, address, buffer);
}

void Device::sendBool(uint16_t address, bool x) {
    union {
        uint8_t buffer[sizeof(uint16_t)];
        uint16_t bval;
    };
    int value = x ? 1 : 0;
    bval = htons(value);
    makeWriteRequest(1, address, buffer);
}

/*!
 * \brief Writes a message to serial.
 *
 * Takes a message, appends the checksum to it, and writes it to the SerialPort.
 *
 * \param size Size of the message before appending the checksum.
 * \param message Message to be written.
 */
void Device::writeMessage(int size, uint8_t* message) {
    // Appends the checksum to the message
    message[size] = computeChecksum(size, message);

    pthread_mutex_lock(&nextWriteMutex);

    if (debugSend) {
        cout << "Device " << id << ": Sending ";
        for (int i = 0; i <= size; ++i) {
            cout << (int) message[i];
            if (i == size) {
                cout << endl;
            }
            else {
                cout << ", ";
            }
        }
    }

    // Check that I'm not writing too fast.
    timeval now;
    gettimeofday(&now, NULL);

    // if NOW < nextWrite, we're not ready to update yet!
    // (while loop in case nextWrite changes while I'm sleeping!)
    while (timercmp(&now, &nextWrite, <)) {
        timeval sleepTime;
        timersub(&nextWrite, &now, &sleepTime);
        usleep(sleepTime.tv_sec * 1000000 + sleepTime.tv_usec);
        // Check now again so I don't infinite loop.
        gettimeofday(&now, NULL);
    }

    // Ensure that serial port reports the expected number of bytes written.
    int bytesWritten = port->writeSer(message, size + 1);
    if (bytesWritten != size + 1) {
        cerr << "Device " << id << ": Write message should have written "
            << size + 1 << " bytes, but instead wrote " << bytesWritten
            << " bytes???" << endl;
        // Fatal - indicates Linux or serial hardware problem!
        // exit(1);
    }
    // How subtle! We must update now because writing to serial takes time!
    gettimeofday(&now, NULL);
    timeradd(&now, &writeIntervalTime, &nextWrite);

    pthread_mutex_unlock(&nextWriteMutex);
}

/*!
 * \brief Checks whether a function is a valid response.
 *
 * \param function Function byte from serial.
 * \return True iff the function is valid (is defined in the serial protocol,
 * and it is possible for the device to send the function to the computer).
 */
bool Device::isValidResponse(uint8_t function) {
    switch (function) {
    case (PACKET_TYPE_READ | PACKET_TYPE_RESPONSE):
    case (PACKET_TYPE_WRITE | PACKET_TYPE_RESPONSE):
    case (PACKET_TYPE_AND | PACKET_TYPE_RESPONSE):
    case (PACKET_TYPE_OR | PACKET_TYPE_RESPONSE):
    case (PACKET_TYPE_XOR | PACKET_TYPE_RESPONSE):
    case PACKET_TYPE_INVALID_REGISTER:
    case PACKET_TYPE_READONLY_REGISTER:
    case PACKET_TYPE_BAD_LENGTH:
    case PACKET_TYPE_BAD_CHECKSUM:
    case PACKET_TYPE_INVALID_FUNCTION:
    case PACKET_TYPE_UNKNOWN_FAILURE:
        return true;
    default:
        return false;
    }

    // Uhm... just in case?
    return false;
}

/*!
 * \brief Static stub function to run the main thread.
 *
 * \param device Pointer to the Device whose main thread should be run.
 */
void* Device::runMainLoop(void* device) {
    Device* realDevice = static_cast<Device*>(device);
    realDevice->mainLoop();
    pthread_exit(NULL);
}

/*!
 * \brief Static stub function to run the read thread.
 *
 * \param device Pointer to the Device whose read thread should be run.
 */
void* Device::runReadLoop(void* device) {
    Device* realDevice = static_cast<Device*>(device);
    realDevice->readLoop();
    pthread_exit(NULL);
}

/*!
 * \brief Static stub function to run the watch thread.
 *
 * \param device Pointer to the Device whose watch thread should be run.
 */
void* Device::runWatchLoop(void* device) {
    Device* realDevice = static_cast<Device*>(device);
    realDevice->watchLoop();
    pthread_exit(NULL);
}

bool Device::ComparePollBlockPointers::
    operator()(const PollBlock* lhs, const PollBlock* rhs) const {

    timeval leftTime = lhs->getNextPollTime();
    timeval rightTime = rhs->getNextPollTime();
    // Compare their times - the smaller time (closer to now)
    // should have higher priority.
    return timercmp(&leftTime, &rightTime, >);
}
