#include <libshm/c/shm.h>

#include "gpio.h"
#include "../filters.h"
#include "../pollblock.h"

Gpio::Gpio(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(10000, 30, 6));
    shm_getg(pressure, pressure);
    shm_getg(depth, depth);
}

void Gpio::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool pressure_changed = false;
    bool depth_changed = false;
    union {
        double vDouble;
        int vInt;
        bool vBool;
    };
    (void)vDouble;
    (void)vInt;
    (void)vBool;

    while (i < count) {
        switch(address + i) {
        case 30:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != depth.offset) {
                // If first change on this group, resync
                if (!depth_changed) {
                    shm_getg(depth, depth);
                }
                depth_changed = true;
                depth.offset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 32:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != depth.depth) {
                // If first change on this group, resync
                if (!depth_changed) {
                    shm_getg(depth, depth);
                }
                depth_changed = true;
                depth.depth = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 34:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != pressure.internal) {
                // If first change on this group, resync
                if (!pressure_changed) {
                    shm_getg(pressure, pressure);
                }
                pressure_changed = true;
                pressure.internal = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (pressure_changed) {
        shm_setg(pressure, pressure);
    }
    if (depth_changed) {
        shm_setg(depth, depth);
    }
}

void Gpio::watch() {
    watcher_t w = create_watcher();
    shm_watch(pressure, w);
    shm_watch(depth, w);

    // Initial write:
    // TODO: resync before this?

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(pressure, pressure_new);
        shm_getg(depth, depth_new);

    }

    destroy_watcher(w);
}
