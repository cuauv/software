#include <iostream>
#include <netinet/in.h>
#include <memory>
#include <map>
#include <fstream>

#include "autodetector.h"
#include "device.h"
#include "protocol.h"

#include "../sensors/serial/serial.h"

#define DEBUG_BAD_PACKETS
#define DEBUG_FOUND_DEVICES

using namespace sensorserial;
using namespace std;

void makeReadRequest(uint8_t count, uint16_t address);
void writeMessage(int size, uint8_t* message);

// How many microseconds to wait before each read
const long SERIAL_WAIT = 100000;
// How many microseconds to wait on each read
const long SERIAL_TIMEOUT = 100000;

// How many bytes and how long to wait when flushing buffer
const int JUNK_LENGTH = 10000;
const int JUNK_TIMEOUT = 100000;

// Base packet, ID length in bytes (each register is 2 bytes), checksum
const int PACKET_SIZE = BASE_PACKET_LENGTH + ID_LENGTH * 2 + 1;

uint16_t Autodetector::detect(const char* path, int baudRate) {
    SerialPort port(path, baudRate);
    if (!port.isOpen()) {
        fprintf(stderr, "Port %s not found\n", path);
        return 0;
    }

    // Make a packet.
    uint8_t message[BASE_PACKET_LENGTH + 1];
    message[0] = PACKET_TYPE_READ;
    message[1] = ID_LENGTH;
    // Be awesome and assign 2 bytes at once!
    *(reinterpret_cast<uint16_t*>(message + 2)) = htons(ID_ADDRESS);
    message[BASE_PACKET_LENGTH] = computeChecksum(BASE_PACKET_LENGTH, message);

    // Try to write it.
    int bytesWritten = port.writeSer(message, BASE_PACKET_LENGTH + 1);
    if (bytesWritten != BASE_PACKET_LENGTH + 1) {
        // Well, it's definitely not this one...
        return 0;
    }

    usleep(SERIAL_WAIT);

    uint8_t buffer[PACKET_SIZE];
    int bytesRead = port.readWithTimeout(buffer, PACKET_SIZE, SERIAL_TIMEOUT);
    if (bytesRead <= 0) {
        return 0;
    }

    uint8_t packetType = buffer[0];
    uint8_t count = buffer[1];
    uint16_t address = ntohs(*(reinterpret_cast<uint16_t*>(buffer + 2)));
    uint8_t checksumReceived = buffer[PACKET_SIZE - 1];
    uint8_t checksumExpected = computeChecksum(PACKET_SIZE - 1, buffer);
    if (packetType != (PACKET_TYPE_READ | PACKET_TYPE_RESPONSE) || address != ID_ADDRESS || count != ID_LENGTH || checksumReceived != checksumExpected) {

#ifdef DEBUG_BAD_PACKETS
        cout << path << ": got ";
        for (int j = 0; j < PACKET_SIZE; ++j) {
            cout << (int) buffer[j] << " ";
        }
        cout << endl;
#endif

        // Clear the buffer.
        uint8_t junk[JUNK_LENGTH];
        port.readWithTimeout(junk, JUNK_LENGTH, JUNK_TIMEOUT);

        // Nope, not this one.
        return 0;
    }

    uint16_t receivedId = ntohs(*(reinterpret_cast<uint16_t*>(buffer + BASE_PACKET_LENGTH)));
    return receivedId;
}

Autodetector::Autodetector(int numports, const char** ports, map<int, Device*> &devices) {
    int baudRate = 0;
    ofstream myfile;
    bool fileDefined = false;
    for (int i = 0; i < numports; ++i) {
        int receivedId = detect(ports[i], 57600);
        if (receivedId == 0) {
            receivedId = detect(ports[i], 230400);
            if (receivedId == 0) {
                continue;
            }
            else {
                baudRate = 230400;
            }
        }
        else {
            baudRate = 57600;
        }
        
        map<int, Device*>::iterator it = devices.find(receivedId);
        if (it != devices.end()) {
          if (!fileDefined) {
            myfile.open("../webserver/serialstatus.txt");
            fileDefined = true;
          }
          myfile << devices[receivedId]->getName() << endl;
#ifdef DEBUG_FOUND_DEVICES
            cout << "Found " << receivedId << " (" << devices[receivedId]->getName() << ") on " << ports[i]
                 << " with baud rate " << baudRate << endl;
#endif
            // FOUND IT!!!
            devices[receivedId]->setPath(ports[i]);
            devices[receivedId]->start();
        }
        else {
            printf("No config file for device with ID %d on port %s\n", receivedId, ports[i]);
        }
    }
    myfile.close();
}
