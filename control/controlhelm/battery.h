#ifndef CUAUV_CONTROL_HELM_BATTERY_H
#define CUAUV_CONTROL_HELM_BATTERY_H

extern const double HELM_BATTERY_LOWV;
extern const double HELM_BATTERY_EMPTYV;

// How many times to average over.
extern const int HELM_BATTERY_N;

// Smoothed voltages.
extern double helm_portv;
extern double helm_starv;
extern double helm_totalv;

// Raw voltages.
extern double helm_portv_;
extern double helm_starv_;
extern double helm_totalv_;

enum class helm_display_battery_code {
    NOMINAL,
    LOW,
    EMPTY,
    DISABLED
};

extern helm_display_battery_code helm_display_battery;

extern bool helm_battery_flash;

void helm_update_battery();

#endif // CUAUV_CONTROL_HELM_BATTERY_H
