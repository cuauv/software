#include <libshm/c/shm.h>

#include "actuators.h"
#include "../filters.h"
#include "../pollblock.h"

Actuators::Actuators(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 33, 5));
    shm_getg(switches, switches);
    shm_getg(actuator_desires, actuator_desires);
    shm_getg(actuator_status, actuator_status);
}

void Actuators::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool switches_changed = false;
    bool actuator_desires_changed = false;
    bool actuator_status_changed = false;
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
            if (vInt != actuator_desires.trigger_01) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_01 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 17:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_02) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_02 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 18:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_03) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_03 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 19:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_04) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_04 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 20:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_05) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_05 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 21:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_06) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_06 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 22:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_07) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_07 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 23:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_08) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_08 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 24:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_09) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_09 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 25:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_10) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_10 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 26:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_11) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_11 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 27:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_12) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_12 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 28:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_13) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_13 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 29:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.trigger_14) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.trigger_14 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 30:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.motor_pwm_1) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.motor_pwm_1 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 31:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_desires.motor_pwm_2) {
                // If first change on this group, resync
                if (!actuator_desires_changed) {
                    shm_getg(actuator_desires, actuator_desires);
                }
                actuator_desires_changed = true;
                actuator_desires.motor_pwm_2 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 32:
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
        case 33:
            vInt = (int16FromBuffer(buffer));
            if (vInt != actuator_status.blown_fuses) {
                // If first change on this group, resync
                if (!actuator_status_changed) {
                    shm_getg(actuator_status, actuator_status);
                }
                actuator_status_changed = true;
                actuator_status.blown_fuses = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 34:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != actuator_status.motor_current_1) {
                // If first change on this group, resync
                if (!actuator_status_changed) {
                    shm_getg(actuator_status, actuator_status);
                }
                actuator_status_changed = true;
                actuator_status.motor_current_1 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 36:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != actuator_status.motor_current_2) {
                // If first change on this group, resync
                if (!actuator_status_changed) {
                    shm_getg(actuator_status, actuator_status);
                }
                actuator_status_changed = true;
                actuator_status.motor_current_2 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (switches_changed) {
        shm_setg(switches, switches);
    }
    if (actuator_status_changed) {
        shm_setg(actuator_status, actuator_status);
    }
}

void Actuators::watch() {
    watcher_t w = create_watcher();
    shm_watch(switches, w);
    shm_watch(actuator_desires, w);
    shm_watch(actuator_status, w);

    // Initial write:
    // TODO: resync before this?
    sendInt16(16, actuator_desires.trigger_01);
    sendInt16(17, actuator_desires.trigger_02);
    sendInt16(18, actuator_desires.trigger_03);
    sendInt16(19, actuator_desires.trigger_04);
    sendInt16(20, actuator_desires.trigger_05);
    sendInt16(21, actuator_desires.trigger_06);
    sendInt16(22, actuator_desires.trigger_07);
    sendInt16(23, actuator_desires.trigger_08);
    sendInt16(24, actuator_desires.trigger_09);
    sendInt16(25, actuator_desires.trigger_10);
    sendInt16(26, actuator_desires.trigger_11);
    sendInt16(27, actuator_desires.trigger_12);
    sendInt16(28, actuator_desires.trigger_13);
    sendInt16(29, actuator_desires.trigger_14);
    sendInt16(30, actuator_desires.motor_pwm_1);
    sendInt16(31, actuator_desires.motor_pwm_2);
    sendInt16(32, switches.soft_kill);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(switches, switches_new);
        shm_getg(actuator_desires, actuator_desires_new);
        shm_getg(actuator_status, actuator_status_new);

        if (actuator_desires_new.trigger_01 != actuator_desires.trigger_01) {
            sendInt16(16, (actuator_desires_new.trigger_01));
            actuator_desires.trigger_01 = actuator_desires_new.trigger_01;
        }
        if (actuator_desires_new.trigger_02 != actuator_desires.trigger_02) {
            sendInt16(17, (actuator_desires_new.trigger_02));
            actuator_desires.trigger_02 = actuator_desires_new.trigger_02;
        }
        if (actuator_desires_new.trigger_03 != actuator_desires.trigger_03) {
            sendInt16(18, (actuator_desires_new.trigger_03));
            actuator_desires.trigger_03 = actuator_desires_new.trigger_03;
        }
        if (actuator_desires_new.trigger_04 != actuator_desires.trigger_04) {
            sendInt16(19, (actuator_desires_new.trigger_04));
            actuator_desires.trigger_04 = actuator_desires_new.trigger_04;
        }
        if (actuator_desires_new.trigger_05 != actuator_desires.trigger_05) {
            sendInt16(20, (actuator_desires_new.trigger_05));
            actuator_desires.trigger_05 = actuator_desires_new.trigger_05;
        }
        if (actuator_desires_new.trigger_06 != actuator_desires.trigger_06) {
            sendInt16(21, (actuator_desires_new.trigger_06));
            actuator_desires.trigger_06 = actuator_desires_new.trigger_06;
        }
        if (actuator_desires_new.trigger_07 != actuator_desires.trigger_07) {
            sendInt16(22, (actuator_desires_new.trigger_07));
            actuator_desires.trigger_07 = actuator_desires_new.trigger_07;
        }
        if (actuator_desires_new.trigger_08 != actuator_desires.trigger_08) {
            sendInt16(23, (actuator_desires_new.trigger_08));
            actuator_desires.trigger_08 = actuator_desires_new.trigger_08;
        }
        if (actuator_desires_new.trigger_09 != actuator_desires.trigger_09) {
            sendInt16(24, (actuator_desires_new.trigger_09));
            actuator_desires.trigger_09 = actuator_desires_new.trigger_09;
        }
        if (actuator_desires_new.trigger_10 != actuator_desires.trigger_10) {
            sendInt16(25, (actuator_desires_new.trigger_10));
            actuator_desires.trigger_10 = actuator_desires_new.trigger_10;
        }
        if (actuator_desires_new.trigger_11 != actuator_desires.trigger_11) {
            sendInt16(26, (actuator_desires_new.trigger_11));
            actuator_desires.trigger_11 = actuator_desires_new.trigger_11;
        }
        if (actuator_desires_new.trigger_12 != actuator_desires.trigger_12) {
            sendInt16(27, (actuator_desires_new.trigger_12));
            actuator_desires.trigger_12 = actuator_desires_new.trigger_12;
        }
        if (actuator_desires_new.trigger_13 != actuator_desires.trigger_13) {
            sendInt16(28, (actuator_desires_new.trigger_13));
            actuator_desires.trigger_13 = actuator_desires_new.trigger_13;
        }
        if (actuator_desires_new.trigger_14 != actuator_desires.trigger_14) {
            sendInt16(29, (actuator_desires_new.trigger_14));
            actuator_desires.trigger_14 = actuator_desires_new.trigger_14;
        }
        if (actuator_desires_new.motor_pwm_1 != actuator_desires.motor_pwm_1) {
            sendInt16(30, (actuator_desires_new.motor_pwm_1));
            actuator_desires.motor_pwm_1 = actuator_desires_new.motor_pwm_1;
        }
        if (actuator_desires_new.motor_pwm_2 != actuator_desires.motor_pwm_2) {
            sendInt16(31, (actuator_desires_new.motor_pwm_2));
            actuator_desires.motor_pwm_2 = actuator_desires_new.motor_pwm_2;
        }
        if (switches_new.soft_kill != switches.soft_kill) {
            sendInt16(32, (switches_new.soft_kill));
            switches.soft_kill = switches_new.soft_kill;
        }
    }

    destroy_watcher(w);
}
