#include "devices.h"
#include "../device.h"

#include "him.h"

void initDevices(std::list<Device*>& devices, std::map<int, Device*>& autoDetectDevices) {
    Device* dev;

    dev = new Him(170, "him", "autodetect", false, false);
    autoDetectDevices[170] = dev;
}
