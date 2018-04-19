#include <math.h>

#include "row1.h"
#include "locations.h"
#include "alarm_vals.h"
#include "battery.h"

Row1::Row1(int x, int y)
{
    //create the window
	motors_win = subwin(stdscr, ROW1_HEIGHT+2, MOTORS_WIDTH+2, y, x);

	des_win = subwin(stdscr, ROW1_HEIGHT+2, DES_WIDTH+2, y, x + MOTORS_WIDTH+2);

	sensors_win = subwin(stdscr, ROW1_HEIGHT+2, SENSORS_WIDTH+2, y, x + MOTORS_WIDTH+DES_WIDTH+4);

	altitude_win = subwin(stdscr, ROW1_HEIGHT+2, ALTITUDE_WIDTH+2, y, x+MOTORS_WIDTH+DES_WIDTH+SENSORS_WIDTH+6);

	switches_win = subwin(stdscr, ROW1_HEIGHT+2, SWITCH_WIDTH+2, y, x+MOTORS_WIDTH+DES_WIDTH+SENSORS_WIDTH+ALTITUDE_WIDTH+8);
}


void Row1::redraw() {
	// box the window
	box(motors_win, 0, 0);

	box(des_win, 0, 0);
	mvwprintw(des_win, 1, DES_LBL_X, "DES HEAD:");
	mvwprintw(des_win, 2, DES_LBL_X, "DES DPTH:");
	mvwprintw(des_win, 3, DES_LBL_X, "DES PTCH:");
	mvwprintw(des_win, 4, DES_LBL_X, "DES ROLL:");
	mvwprintw(des_win, 5, DES_LBL_X, "DES VELX:");
	mvwprintw(des_win, 6, DES_LBL_X, "DES VELY:");

	box(sensors_win, 0, 0);
	mvwprintw(sensors_win, 1, SENSORS_LBL_X, "HEAD:");
	mvwprintw(sensors_win, 2, SENSORS_LBL_X, "DPTH:");
    mvwprintw(sensors_win, 3, SENSORS_LBL_X, "PTCH:");
    mvwprintw(sensors_win, 4, SENSORS_LBL_X, "ROLL:");
    mvwprintw(sensors_win, 5, SENSORS_LBL_X, "VELX:");
    mvwprintw(sensors_win, 6, SENSORS_LBL_X, "VELY:");
	
	box(altitude_win, 0, 0);
	box(switches_win, 0, 0);

	update();
}

void Row1::update() {
    shm_getg(kalman, kalman);
    shm_getg(navigation_desires, desires);
    shm_getg(navigation_settings, navigation_settings);
    shm_getg(motor_desires, motor_desires);
    shm_getg(settings_control, control_settings);
    shm_getg(switches, switches);
    shm_getg(mission_start_switch, mission_start_switch);
    shm_getg(dvl, dvl);

	//--------------------------------------------------
	//
	// Motor Desires
	//
	//--------------------------------------------------
	mvwprintw(motors_win, 1, MOTORS_LBL_X, "PORT:");
	mvwprintw(motors_win, 1, MOTORS_X, "%5d", motor_desires.port);
	
	mvwprintw(motors_win, 2, MOTORS_LBL_X, "STAR:");
	mvwprintw(motors_win, 2, MOTORS_X, "%5d", motor_desires.starboard);
	
	mvwprintw(motors_win, 3, MOTORS_LBL_X, "FORE:");
	mvwprintw(motors_win, 3, MOTORS_X, "%3d:%3d", motor_desires.fore_port,
                                                  motor_desires.fore_starboard);
	
	mvwprintw(motors_win, 4, MOTORS_LBL_X, " AFT:");
	mvwprintw(motors_win, 4, MOTORS_X, "%3d:%3d", motor_desires.aft_port,
                                                   motor_desires.aft_starboard);
	
	mvwprintw(motors_win, 5, MOTORS_LBL_X, "SFOR:");
	mvwprintw(motors_win, 5, MOTORS_X, "%5d", motor_desires.sway_fore);
	
	mvwprintw(motors_win, 6, MOTORS_LBL_X, "SAFT:");
	mvwprintw(motors_win, 6, MOTORS_X, "%5d", motor_desires.sway_aft);

    switch (helm_display_battery) {
    case helm_display_battery_code::EMPTY:
        if (helm_battery_flash) {
            init_pair(2, COLOR_WHITE, COLOR_RED);
        } else {
            init_pair(2, COLOR_WHITE, COLOR_BLACK);
        }
        wbkgd(motors_win, COLOR_PAIR(2));
        break;
    case helm_display_battery_code::LOW:
        init_pair(2, COLOR_BLACK, COLOR_YELLOW);
        wbkgd(motors_win, COLOR_PAIR(2));
        break;
    default:
        wbkgd(motors_win, COLOR_PAIR(0));
        break;
    }
	
	//--------------------------------------------------
	//
	// Desires window
	//
	//--------------------------------------------------

    mvwprintw(des_win, 1, DES_X, "%7.2f", desires.heading);
    mvwprintw(des_win, 2, DES_X, "%7.2f", desires.depth);
    mvwprintw(des_win, 3, DES_X, "%7.2f", desires.pitch);
    mvwprintw(des_win, 4, DES_X, "%7.2f", desires.roll);
    mvwprintw(des_win, 5, DES_X, "%7.2f", desires.speed);
    mvwprintw(des_win, 6, DES_X, "%7.2f", desires.sway_speed);
	
	//--------------------------------------------------
	//
	// Sensors window
	//
	//--------------------------------------------------
	
    mvwprintw(sensors_win, 1, SENSORS_X, "%6.2f", kalman.heading);
    mvwprintw(sensors_win, 2, SENSORS_X, "%6.2f", kalman.depth);
    mvwprintw(sensors_win, 3, SENSORS_X, "%6.2f", kalman.pitch);
    mvwprintw(sensors_win, 4, SENSORS_X, "%6.2f", kalman.roll);
    mvwprintw(sensors_win, 5, SENSORS_X, "%6.2f", kalman.velx);
    mvwprintw(sensors_win, 6, SENSORS_X, "%6.2f", kalman.vely);
	
	if (fabs(kalman.pitch) > PITCH_ALARM) {
		wattron(sensors_win, A_STANDOUT);
		mvwprintw(sensors_win, 3, SENSORS_LBL_X, "PTCH:");
		wattroff(sensors_win, A_STANDOUT);
    } else {
        mvwprintw(sensors_win, 3, SENSORS_LBL_X, "PTCH:");
    }
    mvwprintw(sensors_win, 3, SENSORS_X, "%6.2f", kalman.pitch);

    if (fabs(kalman.roll) > ROLL_ALARM) {
        wattron(sensors_win, A_STANDOUT);
        mvwprintw(sensors_win, 4, SENSORS_LBL_X, "ROLL:");
        wattroff(sensors_win, A_STANDOUT);
    } else {
        mvwprintw(sensors_win, 4, SENSORS_LBL_X, "ROLL:");
    }
    mvwprintw(sensors_win, 4, SENSORS_X, "%6.2f", kalman.roll);    

	//--------------------------------------------------
	//
	// Altitude Window
	//
	//--------------------------------------------------
	if (dvl.savg_altitude > 0 && dvl.savg_altitude < ALTITUDE_ALARM) {
		wattron(altitude_win, A_STANDOUT);
		mvwprintw(altitude_win, 1, ALTITUDE_LBL_X, "DVL ALTD:");
		wattroff(altitude_win, A_STANDOUT);
	} else {
		mvwprintw(altitude_win, 1, ALTITUDE_LBL_X, "DVL ALTD:");
	}
	mvwprintw(altitude_win, 1, ALTITUDE_X, "%6.2f", dvl.savg_altitude);
	
    mvwprintw(altitude_win, 2, ALTITUDE_LBL_X, "DVL TEMP:");
	mvwprintw(altitude_win, 2, ALTITUDE_X, "%6.2f", dvl.temperature);
	
    if (dvl.low_amp_1 || dvl.low_correlation_1) {
        wattron(altitude_win, A_STANDOUT);
        mvwprintw(altitude_win, 3, ALTITUDE_LBL_X, "DVL BEAM 1");
        wattroff(altitude_win, A_STANDOUT);
    } else {
        mvwprintw(altitude_win, 3, ALTITUDE_LBL_X, "DVL BEAM 1");
    }
	
	if (dvl.low_amp_2 || dvl.low_correlation_2) {
        wattron(altitude_win, A_STANDOUT);
        mvwprintw(altitude_win, 4, ALTITUDE_LBL_X, "DVL BEAM 2");
        wattroff(altitude_win, A_STANDOUT);
    } else {
        mvwprintw(altitude_win, 4, ALTITUDE_LBL_X, "DVL BEAM 2");
    }
	
	if (dvl.low_amp_3 || dvl.low_correlation_3) {
        wattron(altitude_win, A_STANDOUT);
        mvwprintw(altitude_win, 5, ALTITUDE_LBL_X, "DVL BEAM 3");
        wattroff(altitude_win, A_STANDOUT);
    } else {
        mvwprintw(altitude_win, 5, ALTITUDE_LBL_X, "DVL BEAM 3");
    }
	
	if (dvl.low_amp_4 || dvl.low_correlation_4) {
        wattron(altitude_win, A_STANDOUT);
        mvwprintw(altitude_win, 6, ALTITUDE_LBL_X, "DVL BEAM 4");
        wattroff(altitude_win, A_STANDOUT);
    } else {
        mvwprintw(altitude_win, 6, ALTITUDE_LBL_X, "DVL BEAM 4");
    }

    // Forward and sway, a bit cramped
    mvwprintw(altitude_win, 3, ALTITUDE_LBL_X + 11, " FWRD:");
    mvwprintw(altitude_win, 4, ALTITUDE_LBL_X + 11, " %0.1f", kalman.forward);
    mvwprintw(altitude_win, 5, ALTITUDE_LBL_X + 11, " SWAY:");
    mvwprintw(altitude_win, 6, ALTITUDE_LBL_X + 11, " %0.1f", kalman.sway);
    
	//--------------------------------------------------
	//
	// Switches Window
	//
	//--------------------------------------------------
	if (switches.hard_kill) {
		wattron(switches_win, A_STANDOUT);
		mvwprintw(switches_win, 1, SWITCHES_X, " HK ");
		wattroff(switches_win, A_STANDOUT);
	} else {
		mvwprintw(switches_win, 1, SWITCHES_X, " HK ");
	}
	
	if (switches.soft_kill) {
		wattron(switches_win, A_STANDOUT);
		mvwprintw(switches_win, 2, SWITCHES_X, " SK ");
		wattroff(switches_win, A_STANDOUT);
	} else {
		mvwprintw(switches_win, 2, SWITCHES_X, " SK ");
	}
	
	if (navigation_settings.position_controls) {
		wattron(switches_win, A_STANDOUT);
		mvwprintw(switches_win, 4, SWITCHES_X, " PC ");
		wattroff(switches_win, A_STANDOUT);
	} else {
		mvwprintw(switches_win, 4, SWITCHES_X, " PC ");
	}
	
	if (navigation_settings.optimize) {
		wattron(switches_win, A_STANDOUT);
		mvwprintw(switches_win, 5, SWITCHES_X, " OT ");
		wattroff(switches_win, A_STANDOUT);
	} else {
		mvwprintw(switches_win, 5, SWITCHES_X, " OT ");
	}
	
	if (control_settings.enabled) {
		wattron(switches_win, A_STANDOUT);
		mvwprintw(switches_win, 6, SWITCHES_X, " EN ");
		wattroff(switches_win, A_STANDOUT);
	} else {
		mvwprintw(switches_win, 6, SWITCHES_X, " EN ");
	}

	wrefresh(motors_win);
	wrefresh(des_win);
	wrefresh(sensors_win);
	wrefresh(altitude_win);
	wrefresh(switches_win);
	
}

