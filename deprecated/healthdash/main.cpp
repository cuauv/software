/* All the headers we need are in here */
#include "common-include.h"

/* This here is the good stuff */
//#include "thrusters.h"
#include "power.h"
#include "computer.h"
#include "other.h"
#include "temperature.h"
#include "libshm/c/shm.h"

using namespace std;

bool done = false;

void draw_main() {
    update_main_batt();
    update_main_distro();
    //update_main_thruster();
    update_main_switches();
    wrefresh(main_batt_status);
    wrefresh(main_distro_status);
    //wrefresh(main_thruster_status);
    wrefresh(main_switches);
    refresh(); /* Moves cursor and draws legend */
}

void draw_power() { 
    update_batt_port();
    update_batt_starboard();
    update_distro_5v();
    update_distro_12v();
    wrefresh(batt_port);
    wrefresh(batt_starboard);
    wrefresh(sensorpwr_5v);
    wrefresh(sensorpwr_12v);
    refresh(); /* Moves cursor */
}

/*
void draw_thruster() { 
    update_thruster_port();
    update_thruster_starboard();
    update_thruster_fore();
    update_thruster_aft();
    update_thruster_overview();
    wrefresh(thrust_port);
    wrefresh(thrust_stbd);
    wrefresh(thrust_fore);
    wrefresh(thrust_aft);
    wrefresh(thrust_oview);
    refresh();
}
*/


void draw_cpu() {
    update_cpu();
    wrefresh(cpu_info);
    refresh();
}

void draw_temp() {
    //update_temperature();
    //wrefresh(temperature_info);
    mvprintw(1,1,"Temparature Window - Not Yet Implemented");
    refresh();
}

/* Cleanup Function - any cleanup code goes here */
void handle_signal(int signal) {
    done = true;
}

void shutdown_display() {
    endwin();
}

/* Draws in stdscr directly. Anything for all pages goes here */
void draw_legend() {
    /* Draw Status */
    move(getmaxy(stdscr)-5,0);
    hline(ACS_HLINE,1000);
    mvprintw(getmaxy(stdscr)-4,1,"Batt: ");
    if (port_batt_present()) {
    	attrset(port_batt_status()); printw("FORE"); attrset(0);
    } else printw("    ");
    printw(" ");
    if (starboard_batt_present()) {
        attrset(starboard_batt_status()); printw("AFT"); attrset(0);
    } else printw("    ");
    printw("   Sensor Power: ");
    attrset(distro_5v_status()); printw(" 5V  "); attrset(0); printw(" ");
    attrset(distro_12v_status()); printw(" 12V "); attrset(0); printw(" ");
/*    printw("   Thrust:  ");
    attrset(port_thrust_status()); printw("PORT"); attrset(0); printw(" ");
    attrset(starboard_thrust_status()); printw("STBD"); attrset(0); printw(" ");
    attrset(fore_thrust_status()); printw("FORE"); attrset(0); printw(" ");
    attrset(aft_thrust_status()); printw("AFT"); attrset(0);
  */
  /* Draw Legend */
    mvprintw(getmaxy(stdscr)-2,3,"(F)1: Overview        (F)2: Power Information                         ");
    mvprintw(getmaxy(stdscr)-1,3,"(F)3: Computer Info   (F)4: Temperature Info      (F)5: Quit          ");
    refresh();
}

void init_display() {
    /* Signal Handler */
    signal(SIGTERM, handle_signal);
    signal(SIGINT, handle_signal);

    /* Set up curses/color */
    initscr();
    noecho();
    nodelay(stdscr,TRUE);
    keypad(stdscr,TRUE);
    start_color();    
    init_pair(1,COLOR_GREEN, COLOR_BLACK);
    init_pair(2,COLOR_YELLOW, COLOR_BLACK);
    init_pair(3,COLOR_RED, COLOR_WHITE);
    init_pair(4,COLOR_CYAN, COLOR_BLACK);

    /* Define Windows - Modify Layout Here */
    main_batt_status = newwin(11,35,0,1);
    main_distro_status = newwin(11,17,0,36);
    //main_thruster_status = newwin(6,14,0,53);
    main_switches = newwin(7,24,11,1);
    batt_port = newwin(20,18,0,1);
    batt_starboard = newwin(20,18,0,20);
    sensorpwr_5v = newwin(7,19,0,40);
    sensorpwr_12v = newwin(7,19,7,40);
    //thrust_port = newwin(9,19,0,1);
    //thrust_stbd = newwin(9,19,9,1);
    //thrust_fore = newwin(9,19,0,21);
    //thrust_aft = newwin(9,19,9,21);
    //thrust_oview = newwin(7,24,0,42);
    cpu_info = newwin(14,27,0,1);
}

int main (int argc, char **argv) {
    void (*to_draw)() = draw_main;
    int key;

    shm_init();

    init_display();

    while(!done) {
        key=getch();
        erase(); /* Only clears stuff on stdscr, not any individual windows */
        draw_legend();
        switch(key) {
            case '1': case KEY_F(1): to_draw = draw_main; break;
            case '2': case KEY_F(2): to_draw = draw_power; break;
            //case '3': case KEY_F(3): to_draw = draw_thruster; break;
            case '3': case KEY_F(3): to_draw = draw_cpu; break;
            case '4': case KEY_F(4): to_draw = draw_temp; break;
    	    case '5': case KEY_F(5): done=1; break;
        }
        to_draw();
        usleep(100000); /* Change frequency (value is period in microseconds) here */
    }
    shutdown_display();
    return 0;
}
