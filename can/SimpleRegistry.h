#pragma once

#include "Registry.h"

class SimpleRegistry : public Registry {
    virtual std::shared_ptr<Device> createDevice(uint8_t devid);
};
