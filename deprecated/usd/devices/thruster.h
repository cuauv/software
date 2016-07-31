#ifndef _USD_THRUSTER_H_
#define _USD_THRUSTER_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Thruster : public Device {
public:
    Thruster(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct switches switches;
    struct switches switches_new;
    struct motor_desires motor_desires;
    struct motor_desires motor_desires_new;
    struct trax trax;
    struct trax trax_new;
};

#endif
