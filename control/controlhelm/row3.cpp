#include "row3.h"
#include "locations.h"
#include "alarm_vals.h"
#include "battery.h"

Row3::Row3(int x, int y)
{
	//create the window
	input_win = subwin(stdscr, 4, RATE_WIDTH+2, y, x+RATE_WIDTH+2);

    bat_win = subwin(stdscr, 4, RATE_WIDTH+2, y, 0);

    placeholder_win = subwin(stdscr, 4, RATE_WIDTH+2, y, x+RATE_WIDTH*2+4);
}

void Row3::redraw() {

	wclear(input_win);
	box(input_win, 0, 0);

    wclear(bat_win);
    box(bat_win, 0, 0);

    box(placeholder_win, 0, 0);

	update();
}

void Row3::update() {
	wrefresh(input_win);

    if (helm_display_battery != helm_display_battery_code::DISABLED) {
        mvwprintw(bat_win, 1, 2, "       %3.2fV       ", helm_totalv_); 
    }

    switch (helm_display_battery) {
    case helm_display_battery_code::EMPTY:
        if (helm_battery_flash) {
            init_pair(2, COLOR_WHITE, COLOR_RED);
        } else {
            init_pair(2, COLOR_WHITE, COLOR_BLACK);
        }
        helm_battery_flash = !helm_battery_flash;
        wbkgd(bat_win, COLOR_PAIR(2));
        mvwprintw(bat_win, 2, 5, "REPLACE BATTERIES");
        break;
    case helm_display_battery_code::LOW:
        init_pair(2, COLOR_BLACK, COLOR_YELLOW);
        wbkgd(bat_win, COLOR_PAIR(2));
        mvwprintw(bat_win, 2, 7, "Low voltages.");
        break;
    case helm_display_battery_code::NOMINAL:
        wbkgd(bat_win, COLOR_PAIR(0));
        mvwprintw(bat_win, 2, 5, "Voltages nominal.");
        break;
    case helm_display_battery_code::DISABLED:
        wbkgd(bat_win, COLOR_PAIR(0));
        mvwprintw(bat_win, 1, 5, "Not on vehicle.");
        mvwprintw(bat_win, 2, 3, "Monitoring disabled.");
    }


    wrefresh(bat_win);
}

WINDOW* Row3::getInputWin() {
	return input_win;
}
