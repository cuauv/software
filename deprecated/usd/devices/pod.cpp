#include <libshm/c/shm.h>

#include "pod.h"
#include "../filters.h"
#include "../pollblock.h"

Pod::Pod(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 16, 14));
    shm_getg(switches, switches);
    shm_getg(mission_start_switch, mission_start_switch);
    shm_getg(merge_status, merge_status);
}

void Pod::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool switches_changed = false;
    bool mission_start_switch_changed = false;
    bool merge_status_changed = false;
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
        case 16:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.total_current) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.total_current = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 18:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.total_voltage) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.total_voltage = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 20:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.current_starboard) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.current_starboard = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 22:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.voltage_starboard) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.voltage_starboard = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 24:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.current_port) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.current_port = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 26:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != merge_status.voltage_port) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.voltage_port = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 28:
            vInt = (int16FromBuffer(buffer));
            if (vInt != merge_status.mission_start) {
                // If first change on this group, resync
                if (!merge_status_changed) {
                    shm_getg(merge_status, merge_status);
                }
                merge_status_changed = true;
                merge_status.mission_start = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 29:
            vInt = unit_hard_kill(int16FromBuffer(buffer));
            if (vInt != switches.hard_kill) {
                // If first change on this group, resync
                if (!switches_changed) {
                    shm_getg(switches, switches);
                }
                switches_changed = true;
                switches.hard_kill = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 30:
            vInt = (int16FromBuffer(buffer));
            if (vInt != mission_start_switch.mission_light) {
                // If first change on this group, resync
                if (!mission_start_switch_changed) {
                    shm_getg(mission_start_switch, mission_start_switch);
                }
                mission_start_switch_changed = true;
                mission_start_switch.mission_light = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (switches_changed) {
        shm_setg(switches, switches);
    }
    if (merge_status_changed) {
        shm_setg(merge_status, merge_status);
    }
}

void Pod::watch() {
    watcher_t w = create_watcher();
    shm_watch(switches, w);
    shm_watch(mission_start_switch, w);
    shm_watch(merge_status, w);

    // Initial write:
    // TODO: resync before this?
    sendInt16(30, mission_start_switch.mission_light);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(switches, switches_new);
        shm_getg(mission_start_switch, mission_start_switch_new);
        shm_getg(merge_status, merge_status_new);

        if (mission_start_switch_new.mission_light != mission_start_switch.mission_light) {
            sendInt16(30, (mission_start_switch_new.mission_light));
            mission_start_switch.mission_light = mission_start_switch_new.mission_light;
        }
    }

    destroy_watcher(w);
}
