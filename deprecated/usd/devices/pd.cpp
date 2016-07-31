#include <libshm/c/shm.h>

#include "pd.h"
#include "../filters.h"
#include "../pollblock.h"

Pd::Pd(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 57600, debugSend, debugReceive) {

    addPollBlock(new PollBlock(100000, 16, 1));
    addPollBlock(new PollBlock(100000, 18, 46));
    shm_getg(sensor_power, sensor_power);
    shm_getg(sensor_power_status, sensor_power_status);
    shm_getg(supply_12v, supply_12v);
    shm_getg(supply_5v, supply_5v);
    shm_getg(supply_24v, supply_24v);
}

void Pd::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool sensor_power_changed = false;
    bool sensor_power_status_changed = false;
    bool supply_12v_changed = false;
    bool supply_5v_changed = false;
    bool supply_24v_changed = false;
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
            if (vInt != sensor_power.desired_port_status) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.desired_port_status = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 18:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_5v.voltage) {
                // If first change on this group, resync
                if (!supply_5v_changed) {
                    shm_getg(supply_5v, supply_5v);
                }
                supply_5v_changed = true;
                supply_5v.voltage = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 20:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_5v.current) {
                // If first change on this group, resync
                if (!supply_5v_changed) {
                    shm_getg(supply_5v, supply_5v);
                }
                supply_5v_changed = true;
                supply_5v.current = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 22:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_12v.voltage) {
                // If first change on this group, resync
                if (!supply_12v_changed) {
                    shm_getg(supply_12v, supply_12v);
                }
                supply_12v_changed = true;
                supply_12v.voltage = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 24:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_12v.current) {
                // If first change on this group, resync
                if (!supply_12v_changed) {
                    shm_getg(supply_12v, supply_12v);
                }
                supply_12v_changed = true;
                supply_12v.current = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 26:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_24v.voltage) {
                // If first change on this group, resync
                if (!supply_24v_changed) {
                    shm_getg(supply_24v, supply_24v);
                }
                supply_24v_changed = true;
                supply_24v.voltage = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 28:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != supply_24v.current) {
                // If first change on this group, resync
                if (!supply_24v_changed) {
                    shm_getg(supply_24v, supply_24v);
                }
                supply_24v_changed = true;
                supply_24v.current = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 30:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_1) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_1 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 32:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_2) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_2 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 34:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_3) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_3 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 36:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_4) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_4 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 38:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_5) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_5 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 40:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_6) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_6 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 42:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_1) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_1 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 44:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_5v_7) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_5v_7 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 46:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_2) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_2 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 48:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_5) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_5 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 50:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_3) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_3 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 52:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_6) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_6 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 54:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_4) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_4 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 56:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_7) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_7 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 58:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_12v_8) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_12v_8 = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 60:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.current_24v) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.current_24v = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 62:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != sensor_power_status.temperature) {
                // If first change on this group, resync
                if (!sensor_power_status_changed) {
                    shm_getg(sensor_power_status, sensor_power_status);
                }
                sensor_power_status_changed = true;
                sensor_power_status.temperature = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 64:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_1) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_1 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 65:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_2) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_2 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 66:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_3) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_3 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 67:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_4) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_4 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 68:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_5) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_5 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 69:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_6) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_6 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 70:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_5v_7) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_5v_7 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 71:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_1) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_1 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 72:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_2) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_2 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 73:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_3) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_3 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 74:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_4) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_4 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 75:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_5) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_5 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 76:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_6) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_6 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 77:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_7) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_7 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 78:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_12v_8) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_12v_8 = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        case 79:
            vInt = (int16FromBuffer(buffer));
            if (vInt != sensor_power.enable_24v) {
                // If first change on this group, resync
                if (!sensor_power_changed) {
                    shm_getg(sensor_power, sensor_power);
                }
                sensor_power_changed = true;
                sensor_power.enable_24v = vInt;
            }
            i += 1;
            buffer += 2;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (sensor_power_status_changed) {
        shm_setg(sensor_power_status, sensor_power_status);
    }
    if (supply_12v_changed) {
        shm_setg(supply_12v, supply_12v);
    }
    if (supply_5v_changed) {
        shm_setg(supply_5v, supply_5v);
    }
    if (supply_24v_changed) {
        shm_setg(supply_24v, supply_24v);
    }
}

void Pd::watch() {
    watcher_t w = create_watcher();
    shm_watch(sensor_power, w);
    shm_watch(sensor_power_status, w);
    shm_watch(supply_12v, w);
    shm_watch(supply_5v, w);
    shm_watch(supply_24v, w);

    // Initial write:
    // TODO: resync before this?
    sendInt16(16, sensor_power.desired_port_status);
    sendInt16(64, sensor_power.enable_5v_1);
    sendInt16(65, sensor_power.enable_5v_2);
    sendInt16(66, sensor_power.enable_5v_3);
    sendInt16(67, sensor_power.enable_5v_4);
    sendInt16(68, sensor_power.enable_5v_5);
    sendInt16(69, sensor_power.enable_5v_6);
    sendInt16(70, sensor_power.enable_5v_7);
    sendInt16(71, sensor_power.enable_12v_1);
    sendInt16(72, sensor_power.enable_12v_2);
    sendInt16(73, sensor_power.enable_12v_3);
    sendInt16(74, sensor_power.enable_12v_4);
    sendInt16(75, sensor_power.enable_12v_5);
    sendInt16(76, sensor_power.enable_12v_6);
    sendInt16(77, sensor_power.enable_12v_7);
    sendInt16(78, sensor_power.enable_12v_8);
    sendInt16(79, sensor_power.enable_24v);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(sensor_power, sensor_power_new);
        shm_getg(sensor_power_status, sensor_power_status_new);
        shm_getg(supply_12v, supply_12v_new);
        shm_getg(supply_5v, supply_5v_new);
        shm_getg(supply_24v, supply_24v_new);

        if (sensor_power_new.desired_port_status != sensor_power.desired_port_status) {
            sendInt16(16, (sensor_power_new.desired_port_status));
            sensor_power.desired_port_status = sensor_power_new.desired_port_status;
        }
        if (sensor_power_new.enable_5v_1 != sensor_power.enable_5v_1) {
            sendInt16(64, (sensor_power_new.enable_5v_1));
            sensor_power.enable_5v_1 = sensor_power_new.enable_5v_1;
        }
        if (sensor_power_new.enable_5v_2 != sensor_power.enable_5v_2) {
            sendInt16(65, (sensor_power_new.enable_5v_2));
            sensor_power.enable_5v_2 = sensor_power_new.enable_5v_2;
        }
        if (sensor_power_new.enable_5v_3 != sensor_power.enable_5v_3) {
            sendInt16(66, (sensor_power_new.enable_5v_3));
            sensor_power.enable_5v_3 = sensor_power_new.enable_5v_3;
        }
        if (sensor_power_new.enable_5v_4 != sensor_power.enable_5v_4) {
            sendInt16(67, (sensor_power_new.enable_5v_4));
            sensor_power.enable_5v_4 = sensor_power_new.enable_5v_4;
        }
        if (sensor_power_new.enable_5v_5 != sensor_power.enable_5v_5) {
            sendInt16(68, (sensor_power_new.enable_5v_5));
            sensor_power.enable_5v_5 = sensor_power_new.enable_5v_5;
        }
        if (sensor_power_new.enable_5v_6 != sensor_power.enable_5v_6) {
            sendInt16(69, (sensor_power_new.enable_5v_6));
            sensor_power.enable_5v_6 = sensor_power_new.enable_5v_6;
        }
        if (sensor_power_new.enable_5v_7 != sensor_power.enable_5v_7) {
            sendInt16(70, (sensor_power_new.enable_5v_7));
            sensor_power.enable_5v_7 = sensor_power_new.enable_5v_7;
        }
        if (sensor_power_new.enable_12v_1 != sensor_power.enable_12v_1) {
            sendInt16(71, (sensor_power_new.enable_12v_1));
            sensor_power.enable_12v_1 = sensor_power_new.enable_12v_1;
        }
        if (sensor_power_new.enable_12v_2 != sensor_power.enable_12v_2) {
            sendInt16(72, (sensor_power_new.enable_12v_2));
            sensor_power.enable_12v_2 = sensor_power_new.enable_12v_2;
        }
        if (sensor_power_new.enable_12v_3 != sensor_power.enable_12v_3) {
            sendInt16(73, (sensor_power_new.enable_12v_3));
            sensor_power.enable_12v_3 = sensor_power_new.enable_12v_3;
        }
        if (sensor_power_new.enable_12v_4 != sensor_power.enable_12v_4) {
            sendInt16(74, (sensor_power_new.enable_12v_4));
            sensor_power.enable_12v_4 = sensor_power_new.enable_12v_4;
        }
        if (sensor_power_new.enable_12v_5 != sensor_power.enable_12v_5) {
            sendInt16(75, (sensor_power_new.enable_12v_5));
            sensor_power.enable_12v_5 = sensor_power_new.enable_12v_5;
        }
        if (sensor_power_new.enable_12v_6 != sensor_power.enable_12v_6) {
            sendInt16(76, (sensor_power_new.enable_12v_6));
            sensor_power.enable_12v_6 = sensor_power_new.enable_12v_6;
        }
        if (sensor_power_new.enable_12v_7 != sensor_power.enable_12v_7) {
            sendInt16(77, (sensor_power_new.enable_12v_7));
            sensor_power.enable_12v_7 = sensor_power_new.enable_12v_7;
        }
        if (sensor_power_new.enable_12v_8 != sensor_power.enable_12v_8) {
            sendInt16(78, (sensor_power_new.enable_12v_8));
            sensor_power.enable_12v_8 = sensor_power_new.enable_12v_8;
        }
        if (sensor_power_new.enable_24v != sensor_power.enable_24v) {
            sendInt16(79, (sensor_power_new.enable_24v));
            sensor_power.enable_24v = sensor_power_new.enable_24v;
        }
    }

    destroy_watcher(w);
}
