#include <libshm/c/shm.h>

#include "him.h"
#include "../filters.h"
#include "../pollblock.h"

Him::Him(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 34, 26));
    shm_getg(him_settings, him_settings);
    shm_getg(him, him);
}

void Him::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool him_settings_changed = false;
    bool him_changed = false;
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
            if (vDouble != him_settings.heading_offset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.heading_offset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 18:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him_settings.pitchoffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.pitchoffset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 20:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him_settings.rolloffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.rolloffset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 22:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him_settings.xacceloffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.xacceloffset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 24:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him_settings.yacceloffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.yacceloffset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 26:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him_settings.zacceloffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.zacceloffset = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 28:
            vInt = (int16FromBuffer(buffer));
            if (vInt != him_settings.xcompoffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.xcompoffset = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 29:
            vInt = (int16FromBuffer(buffer));
            if (vInt != him_settings.ycompoffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.ycompoffset = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 30:
            vInt = (int16FromBuffer(buffer));
            if (vInt != him_settings.zcompoffset) {
                // If first change on this group, resync
                if (!him_settings_changed) {
                    shm_getg(him_settings, him_settings);
                }
                him_settings_changed = true;
                him_settings.zcompoffset = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 34:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.x_accel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.x_accel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 36:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.y_accel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.y_accel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 38:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.z_accel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.z_accel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 40:
            vDouble = -(floatFromBuffer(buffer));
            if (vDouble != him.pitch_vel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.pitch_vel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 42:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.roll_vel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.roll_vel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 44:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.yaw_vel) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.yaw_vel = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 46:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.mag_x) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.mag_x = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 48:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.mag_y) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.mag_y = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 50:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.mag_z) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.mag_z = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 52:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.heading) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.heading = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 54:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.pitch) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.pitch = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 56:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.roll) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.roll = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 58:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != him.gyro_temp) {
                // If first change on this group, resync
                if (!him_changed) {
                    shm_getg(him, him);
                }
                him_changed = true;
                him.gyro_temp = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (him_changed) {
        shm_setg(him, him);
    }
}

void Him::watch() {
    watcher_t w = create_watcher();
    shm_watch(him_settings, w);
    shm_watch(him, w);

    // Initial write:
    // TODO: resync before this?
    sendFloat(16, him_settings.heading_offset);
    sendFloat(18, him_settings.pitchoffset);
    sendFloat(20, him_settings.rolloffset);
    sendFloat(22, him_settings.xacceloffset);
    sendFloat(24, him_settings.yacceloffset);
    sendFloat(26, him_settings.zacceloffset);
    sendInt16(28, him_settings.xcompoffset);
    sendInt16(29, him_settings.ycompoffset);
    sendInt16(30, him_settings.zcompoffset);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(him_settings, him_settings_new);
        shm_getg(him, him_new);

        if (him_settings_new.heading_offset != him_settings.heading_offset) {
            sendFloat(16, (him_settings_new.heading_offset));
            him_settings.heading_offset = him_settings_new.heading_offset;
        }
        if (him_settings_new.pitchoffset != him_settings.pitchoffset) {
            sendFloat(18, (him_settings_new.pitchoffset));
            him_settings.pitchoffset = him_settings_new.pitchoffset;
        }
        if (him_settings_new.rolloffset != him_settings.rolloffset) {
            sendFloat(20, (him_settings_new.rolloffset));
            him_settings.rolloffset = him_settings_new.rolloffset;
        }
        if (him_settings_new.xacceloffset != him_settings.xacceloffset) {
            sendFloat(22, (him_settings_new.xacceloffset));
            him_settings.xacceloffset = him_settings_new.xacceloffset;
        }
        if (him_settings_new.yacceloffset != him_settings.yacceloffset) {
            sendFloat(24, (him_settings_new.yacceloffset));
            him_settings.yacceloffset = him_settings_new.yacceloffset;
        }
        if (him_settings_new.zacceloffset != him_settings.zacceloffset) {
            sendFloat(26, (him_settings_new.zacceloffset));
            him_settings.zacceloffset = him_settings_new.zacceloffset;
        }
        if (him_settings_new.xcompoffset != him_settings.xcompoffset) {
            sendInt16(28, (him_settings_new.xcompoffset));
            him_settings.xcompoffset = him_settings_new.xcompoffset;
        }
        if (him_settings_new.ycompoffset != him_settings.ycompoffset) {
            sendInt16(29, (him_settings_new.ycompoffset));
            him_settings.ycompoffset = him_settings_new.ycompoffset;
        }
        if (him_settings_new.zcompoffset != him_settings.zcompoffset) {
            sendInt16(30, (him_settings_new.zcompoffset));
            him_settings.zcompoffset = him_settings_new.zcompoffset;
        }
    }

    destroy_watcher(w);
}
