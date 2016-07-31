#ifndef _USD_THRUSTER2_H_
#define _USD_THRUSTER2_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Thruster2 : public Device {
public:
    Thruster2(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct switches switches;
    struct switches switches_new;
    struct motor_desires motor_desires;
    struct motor_desires motor_desires_new;
    struct diagnostics_thrusters diagnostics_thrusters;
    struct diagnostics_thrusters diagnostics_thrusters_new;
};

#endif
