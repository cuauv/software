#ifndef _USD_TESTDEVICE_H_
#define _USD_TESTDEVICE_H_

#include <libshm/c/shm.h>

#include "../device.h"

class TestDevice : public Device {
public:
    TestDevice(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
};

#endif
