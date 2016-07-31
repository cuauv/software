#ifndef _USD_@!dev.name.upper()!@_H_
#define _USD_@!dev.name.upper()!@_H_

#include <libshm/c/shm.h>

#include "../device.h"

class @!dev.name.capitalize()!@ : public Device {
public:
    @!dev.name.capitalize()!@(int id, const char* name, const char* devPath, bool debugSend, bool debugReceive);
protected:
    virtual void handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer);
    virtual void watch();
private:
<!--(for group in dev.get_groups())-->
    struct @!group!@ @!group!@;
    struct @!group!@ @!group!@_new;
<!--(end)-->
};

#endif
