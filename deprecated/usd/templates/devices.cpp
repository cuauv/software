#include "devices.h"
#include "../device.h"

<!--(for dev in devs)-->
#include "@!dev.name!@.h"
<!--(end)-->

void initDevices(std::list<Device*>& devices, std::map<int, Device*>& autoDetectDevices) {
    Device* dev;

<!--(for dev in devs)-->
    dev = new @!dev.name.capitalize()!@(@!dev.id!@, "@!dev.name!@", "@!dev.path!@", false, false);
    <!--(if dev.path == 'autodetect')-->
    autoDetectDevices[@!dev.id!@] = dev;
    <!--(else)-->
    devices.push_back(dev);
    <!--(end)-->
<!--(end)-->
}
