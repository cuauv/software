#include <libshm/c/shm.h>

#include "thruster2.h"
#include "../filters.h"
#include "../pollblock.h"

Thruster2::Thruster2(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 32, 2));
    shm_getg(switches, switches);
    shm_getg(motor_desires, motor_desires);
    shm_getg(diagnostics_thrusters, diagnostics_thrusters);
}

void Thruster2::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool switches_changed = false;
    bool motor_desires_changed = false;
    bool diagnostics_thrusters_changed = false;
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
        case 17:
            vInt = (int16FromBuffer(buffer));
            if (vInt != motor_desires.sway_fore) {
                // If first change on this group, resync
                if (!motor_desires_changed) {
                    shm_getg(motor_desires, motor_desires);
                }
                motor_desires_changed = true;
                motor_desires.sway_fore = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 19:
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
        case 32:
            vInt = (int16FromBuffer(buffer));
            if (vInt != diagnostics_thrusters.thruster2_fuse_status) {
                // If first change on this group, resync
                if (!diagnostics_thrusters_changed) {
                    shm_getg(diagnostics_thrusters, diagnostics_thrusters);
                }
                diagnostics_thrusters_changed = true;
                diagnostics_thrusters.thruster2_fuse_status = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 33:
            vInt = (int16FromBuffer(buffer));
            if (vInt != diagnostics_thrusters.thruster2_hard_kill) {
                // If first change on this group, resync
                if (!diagnostics_thrusters_changed) {
                    shm_getg(diagnostics_thrusters, diagnostics_thrusters);
                }
                diagnostics_thrusters_changed = true;
                diagnostics_thrusters.thruster2_hard_kill = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 34:
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
        default:
            ++i;
            buffer += 2;
        }
    }

    if (diagnostics_thrusters_changed) {
        shm_setg(diagnostics_thrusters, diagnostics_thrusters);
    }
}

void Thruster2::watch() {
    watcher_t w = create_watcher();
    shm_watch(switches, w);
    shm_watch(motor_desires, w);
    shm_watch(diagnostics_thrusters, w);

    // Initial write:
    // TODO: resync before this?
    sendInt16(17, motor_desires.sway_fore);
    sendInt16(19, motor_desires.sway_aft);
    sendInt16(34, switches.soft_kill);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(switches, switches_new);
        shm_getg(motor_desires, motor_desires_new);
        shm_getg(diagnostics_thrusters, diagnostics_thrusters_new);

        if (motor_desires_new.sway_fore != motor_desires.sway_fore) {
            sendInt16(17, (motor_desires_new.sway_fore));
            motor_desires.sway_fore = motor_desires_new.sway_fore;
        }
        if (motor_desires_new.sway_aft != motor_desires.sway_aft) {
            sendInt16(19, (motor_desires_new.sway_aft));
            motor_desires.sway_aft = motor_desires_new.sway_aft;
        }
        if (switches_new.soft_kill != switches.soft_kill) {
            sendInt16(34, (switches_new.soft_kill));
            switches.soft_kill = switches_new.soft_kill;
        }
    }

    destroy_watcher(w);
}
