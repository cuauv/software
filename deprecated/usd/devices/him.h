#ifndef _USD_HIM_H_
#define _USD_HIM_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Him : public Device {
public:
    Him(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct him_settings him_settings;
    struct him_settings him_settings_new;
    struct him him;
    struct him him_new;
};

#endif
