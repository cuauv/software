#include <libshm/c/shm.h>

#include "battery.h"

merge_status shm_pod;

const double HELM_BATTERY_LOWV = 14.2;
const double HELM_BATTERY_EMPTYV = 13.0;

// This depends on DISP_PERIOD in controhelm.cpp.
// Currently it's 200000 µs, which means 5 Hz.
const int HELM_BATTERY_N = 32;

double helm_totalv = HELM_BATTERY_LOWV;

double helm_totalv_;

helm_display_battery_code helm_display_battery = helm_display_battery_code::NOMINAL;

double helm_totals = HELM_BATTERY_LOWV * HELM_BATTERY_N;

bool helm_battery_flash = false;

void helm_update_battery()
{
    shm_getg(merge_status, shm_pod);

    // Sanity check to see if we're just running on a developer computer.
    if (shm_pod.total_current == 0 && shm_pod.total_voltage == 0) {
        helm_display_battery = helm_display_battery_code::DISABLED;
        return;
    }

    helm_totalv_ = shm_pod.total_voltage;

    helm_totals += shm_pod.total_voltage - helm_totals / HELM_BATTERY_N;

    helm_totalv = helm_totals / HELM_BATTERY_N;
    if (helm_totalv < HELM_BATTERY_EMPTYV) {
        helm_display_battery = helm_display_battery_code::EMPTY;
    } else if (helm_totalv < HELM_BATTERY_LOWV) {
        helm_display_battery = helm_display_battery_code::LOW;
    } else {
        helm_display_battery = helm_display_battery_code::NOMINAL;
    }
    
}
