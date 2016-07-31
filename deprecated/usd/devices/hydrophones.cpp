#include <libshm/c/shm.h>

#include "hydrophones.h"
#include "../filters.h"
#include "../pollblock.h"

Hydrophones::Hydrophones(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, 230400, debugSend, debugReceive) {

    addPollBlock(new PollBlock(200000, 20, 2));
    addPollBlock(new PollBlock(200000, 26, 26));
    addPollBlock(new PollBlock(200000, 54, 22));
    addPollBlock(new PollBlock(200000, 82, 10));
    shm_getg(hydrophones_results, hydrophones_results);
    shm_getg(hydrophones_settings, hydrophones_settings);
}

void Hydrophones::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
    bool hydrophones_results_changed = false;
    bool hydrophones_settings_changed = false;
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
        case 20:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.uptime) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.uptime = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 26:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.fsSp) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.fsSp = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 28:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.fsActual) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.fsActual = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 30:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.dsp_mode) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.dsp_mode = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 32:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.gain) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.gain = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 34:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.dsp_fault) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.dsp_fault = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 36:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.fault_code) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.fault_code = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 38:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.trackFrequency) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.trackFrequency = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 40:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.trackMagThresh) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.trackMagThresh = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 42:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.trackAngleThresh) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.trackAngleThresh = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 44:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.trackDeadtime) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.trackDeadtime = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 46:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.ping_count) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.ping_count = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 48:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.ping_time) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.ping_time = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 50:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.intensity) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.intensity = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 54:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.heading) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.heading = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 56:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.elevation) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.elevation = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 58:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.phaseX) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.phaseX = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 60:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.phaseY) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.phaseY = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 62:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.phaseZ) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.phaseZ = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 64:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.searchThresh) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.searchThresh = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 66:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.searchStep) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.searchStep = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 68:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.searchDelta) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.searchDelta = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 70:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.searchCenter) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.searchCenter = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 72:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.searchLength) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.searchLength = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 74:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_results.search_bins) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.search_bins = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 82:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_results.search_count) {
                // If first change on this group, resync
                if (!hydrophones_results_changed) {
                    shm_getg(hydrophones_results, hydrophones_results);
                }
                hydrophones_results_changed = true;
                hydrophones_results.search_count = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 84:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.agEnabled) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.agEnabled = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 86:
            vInt = (int32FromBuffer(buffer));
            if (vInt != hydrophones_settings.agPeriod) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.agPeriod = vInt;
            }
            i += 2;
            buffer += 4;
            break;
        case 88:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.agLow) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.agLow = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        case 90:
            vDouble = (floatFromBuffer(buffer));
            if (vDouble != hydrophones_settings.agHigh) {
                // If first change on this group, resync
                if (!hydrophones_settings_changed) {
                    shm_getg(hydrophones_settings, hydrophones_settings);
                }
                hydrophones_settings_changed = true;
                hydrophones_settings.agHigh = vDouble;
            }
            i += 2;
            buffer += 4;
            break;
        default:
            ++i;
            buffer += 2;
        }
    }

    if (hydrophones_results_changed) {
        shm_setg(hydrophones_results, hydrophones_results);
    }
}

void Hydrophones::watch() {
    watcher_t w = create_watcher();
    shm_watch(hydrophones_results, w);
    shm_watch(hydrophones_settings, w);

    // Initial write:
    // TODO: resync before this?
    sendInt32(20, hydrophones_results.uptime);
    sendFloat(26, hydrophones_settings.fsSp);
    sendFloat(28, hydrophones_results.fsActual);
    sendInt32(30, hydrophones_settings.dsp_mode);
    sendInt32(32, hydrophones_settings.gain);
    sendInt32(34, hydrophones_results.dsp_fault);
    sendInt32(36, hydrophones_results.fault_code);
    sendFloat(38, hydrophones_settings.trackFrequency);
    sendFloat(40, hydrophones_settings.trackMagThresh);
    sendFloat(42, hydrophones_settings.trackAngleThresh);
    sendInt32(44, hydrophones_settings.trackDeadtime);
    sendInt32(46, hydrophones_results.ping_count);
    sendInt32(48, hydrophones_results.ping_time);
    sendFloat(50, hydrophones_results.intensity);
    sendFloat(54, hydrophones_results.heading);
    sendFloat(56, hydrophones_results.elevation);
    sendFloat(58, hydrophones_results.phaseX);
    sendFloat(60, hydrophones_results.phaseY);
    sendFloat(62, hydrophones_results.phaseZ);
    sendFloat(64, hydrophones_settings.searchThresh);
    sendInt32(66, hydrophones_settings.searchStep);
    sendInt32(68, hydrophones_settings.searchDelta);
    sendInt32(70, hydrophones_settings.searchCenter);
    sendInt32(72, hydrophones_settings.searchLength);
    sendInt32(74, hydrophones_results.search_bins);
    sendFloat(82, hydrophones_results.search_count);
    sendInt32(84, hydrophones_settings.agEnabled);
    sendInt32(86, hydrophones_settings.agPeriod);
    sendFloat(88, hydrophones_settings.agLow);
    sendFloat(90, hydrophones_settings.agHigh);

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
        shm_getg(hydrophones_results, hydrophones_results_new);
        shm_getg(hydrophones_settings, hydrophones_settings_new);

        if (hydrophones_results_new.uptime != hydrophones_results.uptime) {
            sendInt32(20, (hydrophones_results_new.uptime));
            hydrophones_results.uptime = hydrophones_results_new.uptime;
        }
        if (hydrophones_settings_new.fsSp != hydrophones_settings.fsSp) {
            sendFloat(26, (hydrophones_settings_new.fsSp));
            hydrophones_settings.fsSp = hydrophones_settings_new.fsSp;
        }
        if (hydrophones_results_new.fsActual != hydrophones_results.fsActual) {
            sendFloat(28, (hydrophones_results_new.fsActual));
            hydrophones_results.fsActual = hydrophones_results_new.fsActual;
        }
        if (hydrophones_settings_new.dsp_mode != hydrophones_settings.dsp_mode) {
            sendInt32(30, (hydrophones_settings_new.dsp_mode));
            hydrophones_settings.dsp_mode = hydrophones_settings_new.dsp_mode;
        }
        if (hydrophones_settings_new.gain != hydrophones_settings.gain) {
            sendInt32(32, (hydrophones_settings_new.gain));
            hydrophones_settings.gain = hydrophones_settings_new.gain;
        }
        if (hydrophones_results_new.dsp_fault != hydrophones_results.dsp_fault) {
            sendInt32(34, (hydrophones_results_new.dsp_fault));
            hydrophones_results.dsp_fault = hydrophones_results_new.dsp_fault;
        }
        if (hydrophones_results_new.fault_code != hydrophones_results.fault_code) {
            sendInt32(36, (hydrophones_results_new.fault_code));
            hydrophones_results.fault_code = hydrophones_results_new.fault_code;
        }
        if (hydrophones_settings_new.trackFrequency != hydrophones_settings.trackFrequency) {
            sendFloat(38, (hydrophones_settings_new.trackFrequency));
            hydrophones_settings.trackFrequency = hydrophones_settings_new.trackFrequency;
        }
        if (hydrophones_settings_new.trackMagThresh != hydrophones_settings.trackMagThresh) {
            sendFloat(40, (hydrophones_settings_new.trackMagThresh));
            hydrophones_settings.trackMagThresh = hydrophones_settings_new.trackMagThresh;
        }
        if (hydrophones_settings_new.trackAngleThresh != hydrophones_settings.trackAngleThresh) {
            sendFloat(42, (hydrophones_settings_new.trackAngleThresh));
            hydrophones_settings.trackAngleThresh = hydrophones_settings_new.trackAngleThresh;
        }
        if (hydrophones_settings_new.trackDeadtime != hydrophones_settings.trackDeadtime) {
            sendInt32(44, (hydrophones_settings_new.trackDeadtime));
            hydrophones_settings.trackDeadtime = hydrophones_settings_new.trackDeadtime;
        }
        if (hydrophones_results_new.ping_count != hydrophones_results.ping_count) {
            sendInt32(46, (hydrophones_results_new.ping_count));
            hydrophones_results.ping_count = hydrophones_results_new.ping_count;
        }
        if (hydrophones_results_new.ping_time != hydrophones_results.ping_time) {
            sendInt32(48, (hydrophones_results_new.ping_time));
            hydrophones_results.ping_time = hydrophones_results_new.ping_time;
        }
        if (hydrophones_results_new.intensity != hydrophones_results.intensity) {
            sendFloat(50, (hydrophones_results_new.intensity));
            hydrophones_results.intensity = hydrophones_results_new.intensity;
        }
        if (hydrophones_results_new.heading != hydrophones_results.heading) {
            sendFloat(54, (hydrophones_results_new.heading));
            hydrophones_results.heading = hydrophones_results_new.heading;
        }
        if (hydrophones_results_new.elevation != hydrophones_results.elevation) {
            sendFloat(56, (hydrophones_results_new.elevation));
            hydrophones_results.elevation = hydrophones_results_new.elevation;
        }
        if (hydrophones_results_new.phaseX != hydrophones_results.phaseX) {
            sendFloat(58, (hydrophones_results_new.phaseX));
            hydrophones_results.phaseX = hydrophones_results_new.phaseX;
        }
        if (hydrophones_results_new.phaseY != hydrophones_results.phaseY) {
            sendFloat(60, (hydrophones_results_new.phaseY));
            hydrophones_results.phaseY = hydrophones_results_new.phaseY;
        }
        if (hydrophones_results_new.phaseZ != hydrophones_results.phaseZ) {
            sendFloat(62, (hydrophones_results_new.phaseZ));
            hydrophones_results.phaseZ = hydrophones_results_new.phaseZ;
        }
        if (hydrophones_settings_new.searchThresh != hydrophones_settings.searchThresh) {
            sendFloat(64, (hydrophones_settings_new.searchThresh));
            hydrophones_settings.searchThresh = hydrophones_settings_new.searchThresh;
        }
        if (hydrophones_settings_new.searchStep != hydrophones_settings.searchStep) {
            sendInt32(66, (hydrophones_settings_new.searchStep));
            hydrophones_settings.searchStep = hydrophones_settings_new.searchStep;
        }
        if (hydrophones_settings_new.searchDelta != hydrophones_settings.searchDelta) {
            sendInt32(68, (hydrophones_settings_new.searchDelta));
            hydrophones_settings.searchDelta = hydrophones_settings_new.searchDelta;
        }
        if (hydrophones_settings_new.searchCenter != hydrophones_settings.searchCenter) {
            sendInt32(70, (hydrophones_settings_new.searchCenter));
            hydrophones_settings.searchCenter = hydrophones_settings_new.searchCenter;
        }
        if (hydrophones_settings_new.searchLength != hydrophones_settings.searchLength) {
            sendInt32(72, (hydrophones_settings_new.searchLength));
            hydrophones_settings.searchLength = hydrophones_settings_new.searchLength;
        }
        if (hydrophones_results_new.search_bins != hydrophones_results.search_bins) {
            sendInt32(74, (hydrophones_results_new.search_bins));
            hydrophones_results.search_bins = hydrophones_results_new.search_bins;
        }
        if (hydrophones_results_new.search_count != hydrophones_results.search_count) {
            sendFloat(82, (hydrophones_results_new.search_count));
            hydrophones_results.search_count = hydrophones_results_new.search_count;
        }
        if (hydrophones_settings_new.agEnabled != hydrophones_settings.agEnabled) {
            sendInt32(84, (hydrophones_settings_new.agEnabled));
            hydrophones_settings.agEnabled = hydrophones_settings_new.agEnabled;
        }
        if (hydrophones_settings_new.agPeriod != hydrophones_settings.agPeriod) {
            sendInt32(86, (hydrophones_settings_new.agPeriod));
            hydrophones_settings.agPeriod = hydrophones_settings_new.agPeriod;
        }
        if (hydrophones_settings_new.agLow != hydrophones_settings.agLow) {
            sendFloat(88, (hydrophones_settings_new.agLow));
            hydrophones_settings.agLow = hydrophones_settings_new.agLow;
        }
        if (hydrophones_settings_new.agHigh != hydrophones_settings.agHigh) {
            sendFloat(90, (hydrophones_settings_new.agHigh));
            hydrophones_settings.agHigh = hydrophones_settings_new.agHigh;
        }
    }

    destroy_watcher(w);
}
