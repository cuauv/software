#ifndef _USD_HYDROPHONES_H_
#define _USD_HYDROPHONES_H_

#include <libshm/c/shm.h>

#include "../device.h"

class Hydrophones : public Device {
public:
    Hydrophones(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
    struct hydrophones_results hydrophones_results;
    struct hydrophones_results hydrophones_results_new;
    struct hydrophones_settings hydrophones_settings;
    struct hydrophones_settings hydrophones_settings_new;
};

#endif
