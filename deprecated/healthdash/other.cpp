#include "other.h"
#include "libshm/c/shm.h"

using namespace std;

/* Windows */
WINDOW *main_switches;

void update_main_switches() {
    struct switches switches;
    struct mission_start_switch mission_start_switch;
    shm_getg(switches, switches);
    shm_getg(mission_start_switch, mission_start_switch);

    wborder(main_switches,0,0,0,0,0,0,0,0);

    mvwprintw(main_switches,1,4,"Switches Status");

    /* Actuator Kill */
    mvwprintw(main_switches,2,2,"Act  Kill:  ");
    wattrset(main_switches, ATTR_ERR);
    wprintw(main_switches, "  Usprtd");
    wattrset(main_switches, 0);
    
    /* Hard Kill */
    mvwprintw(main_switches,3,2,"Hard Kill:  ");
    wattrset(main_switches, (switches.hard_kill) ? ATTR_ERR : ATTR_OK );
    wprintw(main_switches,(switches.hard_kill) ? "  Killed" : "Unkilled");
    wattrset(main_switches, 0);
   
    /* Soft Kill */
    mvwprintw(main_switches,4,2,"Soft Kill:  ");
    wattrset(main_switches, (switches.soft_kill) ? ATTR_ERR : ATTR_OK );
    wprintw(main_switches,(switches.soft_kill) ? "  Killed" : "Unkilled");
    wattrset(main_switches, 0);

    /* Soft Kill */
    mvwprintw(main_switches,5,2,"Mission:    ");
    wattrset(main_switches, (mission_start_switch.mission_start) ? ATTR_WARN : ATTR_OK );
    wprintw(main_switches,(mission_start_switch.mission_start) ? "Running " : "   OFF  ");
    wattrset(main_switches, 0);

    move(getmaxy(stdscr)-1,getmaxx(stdscr)-1);
}
