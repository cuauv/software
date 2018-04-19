#ifndef __ROW1_H__
#define __ROW1_H__

#include <curses.h>
#include "libshm/c/shm.h"
#include "displayable.h"

class Row1 : public Displayable {
    public:
        Row1(int x, int y);
        ~Row1() {};

        void redraw();
        void update();

    private:
        struct kalman kalman;
        struct navigation_desires desires;
        struct motor_desires motor_desires;
        struct settings_control control_settings;
        struct switches switches;
        struct mission_start_switch mission_start_switch;
        struct navigation_settings navigation_settings;
        struct dvl dvl;

        WINDOW *motors_win, *des_win, *sensors_win, *altitude_win, *switches_win;
};

#endif

