#include <libshm/c/shm.h>

#include "thruster.h"
#include "../filters.h"
#include "../pollblock.h"

Thruster::Thruster(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 25, 1));
    shm_getg(switches, switches);
    shm_getg(motor_desires, motor_desires);
    shm_getg(trax, trax);
}

void Thruster::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool switches_changed = false;
    bool motor_desires_changed = false;
    bool trax_changed = false;
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
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.fore_starboard) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.fore_starboard = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 17:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.aft_starboard) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.aft_starboard = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 18:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.starboard) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.starboard = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 19:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.fore_port) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.fore_port = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 20:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.aft_port) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.aft_port = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 21:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.port) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.port = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 22:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.sway_fore) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.sway_fore = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 23:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.sway_aft) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.sway_aft = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 24:
            vInt = (int16FromBuffer(buffer));
            if (vInt != switches.soft_kill) {
                // If first change on this group, resync
                if (!switches_changed) {
                    shm_getg(switches, switches);
                }
                switches_changed = true;
                switches.soft_kill = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 25:
            vInt = (int16FromBuffer(buffer));
            if (vInt != trax.calibrated) {
                // If first change on this group, resync
                if (!trax_changed) {
                    shm_getg(trax, trax);
                }
                trax_changed = true;
                trax.calibrated = vInt;
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
    if (trax_changed) {
        shm_setg(trax, trax);
    }
}

void Thruster::watch() {
    watcher_t w = create_watcher();
    shm_watch(switches, w);
    shm_watch(motor_desires, w);
    shm_watch(trax, w);

    // Initial write:
    // TODO: resync before this?
    sendInt16(16, motor_desires.fore_starboard);
    sendInt16(17, motor_desires.aft_starboard);
    sendInt16(18, motor_desires.starboard);
    sendInt16(19, motor_desires.fore_port);
    sendInt16(20, motor_desires.aft_port);
    sendInt16(21, motor_desires.port);
    sendInt16(22, motor_desires.sway_fore);
    sendInt16(23, motor_desires.sway_aft);
    sendInt16(24, switches.soft_kill);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(switches, switches_new);
        shm_getg(motor_desires, motor_desires_new);
        shm_getg(trax, trax_new);

        if (motor_desires_new.fore_starboard != motor_desires.fore_starboard) {
            sendInt16(16, (motor_desires_new.fore_starboard));
            motor_desires.fore_starboard = motor_desires_new.fore_starboard;
        }
        if (motor_desires_new.aft_starboard != motor_desires.aft_starboard) {
            sendInt16(17, (motor_desires_new.aft_starboard));
            motor_desires.aft_starboard = motor_desires_new.aft_starboard;
        }
        if (motor_desires_new.starboard != motor_desires.starboard) {
            sendInt16(18, (motor_desires_new.starboard));
            motor_desires.starboard = motor_desires_new.starboard;
        }
        if (motor_desires_new.fore_port != motor_desires.fore_port) {
            sendInt16(19, (motor_desires_new.fore_port));
            motor_desires.fore_port = motor_desires_new.fore_port;
        }
        if (motor_desires_new.aft_port != motor_desires.aft_port) {
            sendInt16(20, (motor_desires_new.aft_port));
            motor_desires.aft_port = motor_desires_new.aft_port;
        }
        if (motor_desires_new.port != motor_desires.port) {
            sendInt16(21, (motor_desires_new.port));
            motor_desires.port = motor_desires_new.port;
        }
        if (motor_desires_new.sway_fore != motor_desires.sway_fore) {
            sendInt16(22, (motor_desires_new.sway_fore));
            motor_desires.sway_fore = motor_desires_new.sway_fore;
        }
        if (motor_desires_new.sway_aft != motor_desires.sway_aft) {
            sendInt16(23, (motor_desires_new.sway_aft));
            motor_desires.sway_aft = motor_desires_new.sway_aft;
        }
        if (switches_new.soft_kill != switches.soft_kill) {
            sendInt16(24, (switches_new.soft_kill));
            switches.soft_kill = switches_new.soft_kill;
        }
    }

    destroy_watcher(w);
}
