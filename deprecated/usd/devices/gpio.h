#ifndef _USD_GPIO_H_
#define _USD_GPIO_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Gpio : public Device {
public:
    Gpio(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct pressure pressure;
    struct pressure pressure_new;
    struct depth depth;
    struct depth depth_new;
};

#endif
