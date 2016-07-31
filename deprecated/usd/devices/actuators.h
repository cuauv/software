#ifndef _USD_ACTUATORS_H_
#define _USD_ACTUATORS_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Actuators : public Device {
public:
    Actuators(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct switches switches;
    struct switches switches_new;
    struct actuator_desires actuator_desires;
    struct actuator_desires actuator_desires_new;
    struct actuator_status actuator_status;
    struct actuator_status actuator_status_new;
};

#endif
