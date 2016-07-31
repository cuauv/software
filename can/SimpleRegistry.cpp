#include "SimpleRegistry.h"

#include "lib/Log.h"

std::shared_ptr<Device> SimpleRegistry::createDevice(uint8_t devid) {
    // canlib assigns devid 63 to any device which has not had its EEPROM programmed
    if (devid == 63) {
        Log::log("Disabling unprogrammed device 63");
        auto dev = std::make_shared<Device>(devid, 1, 0);
        addDevice(dev);
        return dev;
    }
    Log::log("Creating device " + std::to_string(devid));
    uint8_t group = (devid-1) / 4 + 2;
    uint8_t offset = (devid-1) % 4;
    if (!getGroup(group)) {
        Log::log("Creating group " + std::to_string(group));
        addGroup(std::make_shared<Group>(group));
    }
    auto dev = std::make_shared<Device>(devid, group, offset);
    addDevice(dev);
    return dev;
}
