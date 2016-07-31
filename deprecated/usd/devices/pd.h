#ifndef _USD_PD_H_
#define _USD_PD_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Pd : public Device {
public:
    Pd(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct sensor_power sensor_power;
    struct sensor_power sensor_power_new;
    struct sensor_power_status sensor_power_status;
    struct sensor_power_status sensor_power_status_new;
    struct supply_12v supply_12v;
    struct supply_12v supply_12v_new;
    struct supply_5v supply_5v;
    struct supply_5v supply_5v_new;
    struct supply_24v supply_24v;
    struct supply_24v supply_24v_new;
};

#endif
