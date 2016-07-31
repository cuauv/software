#include <libshm/c/shm.h>
#include <iostream>

#include "testDevice.h"
#include "../filters.h"
#include "../pollblock.h"

TestDevice::TestDevice(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {
}

void TestDevice::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    std::cout << "Got response. Addr = " << address << ", count = " << count << std::endl;
}

void TestDevice::watch() {
    while (isActive()) {
        // Do something
    }
}
