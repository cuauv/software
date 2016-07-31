#ifndef _USD_POD_H_
#define _USD_POD_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Pod : public Device {
public:
    Pod(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct switches switches;
    struct switches switches_new;
    struct mission_start_switch mission_start_switch;
    struct mission_start_switch mission_start_switch_new;
    struct merge_status merge_status;
    struct merge_status merge_status_new;
};

#endif
