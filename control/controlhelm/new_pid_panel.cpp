#include "new_pid_panel.h"

using namespace std;

//x and y are the top left coordinates of this box
//variable is the name of the variable this PID loop is run on (e.g., depth, heading, etc) and
//should conform to a value of /control/internal/[variable]
//value_var is the name of the auval variable containing the raw input to the controller
//desire_var is the name of the desire variable
NewPidPanel::NewPidPanel(int x, int y, const string& aName,
        ControlVarPointers *cvp) {	
	
    memcpy(&vars, cvp, sizeof(ControlVarPointers));
	
    //create the window
	win = subwin(stdscr, WIN_HEIGHT+2, WIN_WIDTH+2, y, x);
	
	name = aName;
	
	redraw();
}

//udpates the pid panel
void NewPidPanel::update() {
	// Print basic parameters
	mvwprintw(win, ERR_Y, ERR_X, "%7.2f", *vars.desiredValue);
	mvwprintw(win, VAL_Y, VAL_X, "%7.2f", *vars.value);
	mvwprintw(win, OUT_Y, OUT_X, "%7.2f", *vars.out);
	mvwprintw(win, INT_Y, OUT_X, "%7.2f", *vars.rate);

	// send out P, I, D, RD, and MR weight values
	mvwprintw(win, KP_Y, KP_X, "%7.2f", *vars.kP);
	mvwprintw(win, KI_Y, KI_X, "%7.2f", *vars.kI);
	mvwprintw(win, KD_Y, KD_X, "%7.2f", *vars.kD);
	mvwprintw(win, KMR_Y, KMR_X, "%7.2f", *vars.integral);
    if (*vars.locked) {
		wattron(win, A_STANDOUT);
        mvwprintw(win, KRD_Y, KRD_X, "%7.2f", *vars.rD);
		wattroff(win, A_STANDOUT);
    }
    else {
        mvwprintw(win, KRD_Y, KRD_X, "%7.2f", *vars.rD);
    }

	//set on/off
	if (*vars.active) {
		wattron(win, A_STANDOUT);
		mvwprintw(win, ON_Y, ON_X, "ON");
		wattroff(win, A_STANDOUT);
		mvwprintw(win, OFF_Y, OFF_X, "OFF");
	} else {
		mvwprintw(win, ON_Y, ON_X, "ON");
		wattron(win, A_STANDOUT);
		mvwprintw(win, OFF_Y, OFF_X, "OFF");
		wattroff(win, A_STANDOUT);
	}

	touchwin(win);
	wrefresh(win);
}

//handles inputs
void NewPidPanel::HandleInput() {

}

bool NewPidPanel::getActive() {
    return *vars.active;
}

//re-draws pid in the event of a re-draw command
void NewPidPanel::redraw() {
	// box the window
	box(win, 0, 0);
	
	// do the initial display
	mvwprintw(win, LABEL_Y, LABEL_X, name.c_str());
	mvwprintw(win, ERR_Y, ERR_X-4, "DES:");	
	mvwprintw(win, VAL_Y, ERR_X-4, "VAL:");	
	mvwprintw(win, OUT_Y, ERR_X-4, "OUT:");	
	mvwprintw(win, INT_Y, INT_X-4, "RTE:");	
	
	mvwprintw(win, P_Y, P_X, "P:");
	mvwprintw(win, I_Y, I_X, "I:");
	mvwprintw(win, D_Y, D_X, "D:");
	mvwprintw(win, MR_Y, MR_X, "IG:");
	mvwprintw(win, RD_Y, RD_X, "RD:");

	mvwprintw(win, ON_Y, ON_X, "ON");
	mvwprintw(win, OFF_Y, OFF_X, "OFF");

	// update the screen
	touchwin(win);
	wrefresh(win);
	
	// print some values
	update();
}

