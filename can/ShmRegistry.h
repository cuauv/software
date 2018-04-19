#pragma once

#include "Registry.h"

class ShmRegistry : public Registry {
    public:
    ShmRegistry();

    virtual std::shared_ptr<Device> createDevice(uint8_t devid);
};
