#ifndef HD_POWER_H
#define HD_POWER_H

#include "common-include.h"

using namespace std;

/* Battery Windows */
extern WINDOW *main_batt_status;
extern WINDOW *main_distro_status;
extern WINDOW *batt_port;
extern WINDOW *batt_starboard;

/* Distro Windows */
extern WINDOW *sensorpwr_5v;
extern WINDOW *sensorpwr_12v;

/* Battery Status Functions */
int port_batt_status();
int starboard_batt_status();

/* Battery Presence */
bool port_batt_present();
bool starboard_batt_present();

/* Distro Status Functions */
int distro_5v_status();
int distro_12v_status();

/* Main Page Windows */
void update_main_distro();
void update_main_batt();

/* Power Page Battery Windows */
void update_batt_port();
void update_batt_starboard();

/* Power Page Distro Windows */
void update_distro_5v();
void update_distro_12v();
void update_distro_hydro();
void update_distro_stack();

#endif /* HD_POWER_H */
