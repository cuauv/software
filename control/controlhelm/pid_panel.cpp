#include "pid_panel.h"
#include "locations.h"

#include <curses.h>
#include <string>

using namespace std;
using namespace AUV;
using namespace AUV::controld2;

//x and y are the top left coordinates of this box
//variable is the name of the variable this PID loop is run on (e.g., depth, heading, etc) and
//should conform to a value of /control/internal/[variable]
//value_var is the name of the auval variable containing the raw input to the controller
//desire_var is the name of the desire variable
PidPanel::PidPanel(int x, int y, const string& variable, const string& value_var, const string& desire_var) {	
	string base;
	
	//create the window
	win = subwin(stdscr, WIN_HEIGHT+2, WIN_WIDTH+2, y, x);
	
	name = variable;
	
	//First, initialize the variables:
	value = new ShmSharedVar<double>(value_var);
	desire = new ShmSharedVar<double>(desire_var);
	
	//next, access to the controller internals
	base = "/control/internal/";
	base.append(variable);
	base.append("/");
	
	integral = new ShmSharedVar<double>(base + "int");	
	last_err = new ShmSharedVar<double>(base + "last_err");
	out = new ShmSharedVar<double>(base + "out");
	
	//next, access to the settings:
	base = "/settings/control/";
	base.append(variable);
	base.append("/");
	
	active = new ShmSharedVar<int>(base + "active");
	
	kP = new ShmSharedVar<double>(base + "kP");
	kI = new ShmSharedVar<double>(base + "kI");
	kD = new ShmSharedVar<double>(base + "kD");
	kMR = new ShmSharedVar<double>(base + "MR");
	kRD = new ShmSharedVar<double>(base + "RD");

	locked = new ShmSharedVar<int>("/control/locked/" + variable);
	
	RedrawPanel();
}

PidPanel::~PidPanel() {
	delete value;
	delete desire;
	delete integral;
	delete last_err;
	delete out;
	delete active;
	delete locked;
	delete kP;
	delete kI;
	delete kD;
	delete kMR;
	delete kRD;
}

//udpates the panel
void PidPanel::UpdatePanel() {
	// Print basic parameters
	mvwprintw(win, ERR_Y, ERR_X, "%7.2f", last_err->value());
	mvwprintw(win, VAL_Y, VAL_X, "%7.2f", value->value());
	mvwprintw(win, OUT_Y, OUT_X, "%7.2f", out->value());
	mvwprintw(win, INT_Y, INT_X, "%7.2f", integral->value());

	// send out P, I, D, RD, and MR weight values
	mvwprintw(win, KP_Y, KP_X, "%7.2f", kP->value());
	mvwprintw(win, KI_Y, KI_X, "%7.2f", kI->value());
	mvwprintw(win, KD_Y, KD_X, "%7.2f", kD->value());
	mvwprintw(win, KMR_Y, KMR_X, "%7.2f", kMR->value());
	mvwprintw(win, KRD_Y, KRD_X, "%7.2f", kRD->value());

	//set on/off
	if (active->value()) {
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
	
	if (locked->value()) {
		wattron(win, A_STANDOUT);
		mvwprintw(win, LOCKED_Y, LOCKED_X, "LOCKED");
		wattroff(win, A_STANDOUT);
	} else {
		mvwprintw(win, LOCKED_Y, LOCKED_X, "LOCKED");
	}
	
	touchwin(win);
	wrefresh(win);
}

//handles inputs
void PidPanel::HandleInput() {

}

void PidPanel::toggle() {
	if (active->value()) {
		active->set(0);
	} else {
		active->set(1);
	}
}

//re-draws in the event of a re-draw command
void PidPanel::RedrawPanel() {
	// box the window
	box(win, 0, 0);
	
	// do the initial display
	mvwprintw(win, LABEL_Y, LABEL_X, name.c_str());
	mvwprintw(win, ERR_Y, ERR_X-4, "ERR:");	
	mvwprintw(win, VAL_Y, ERR_X-4, "VAL:");	
	mvwprintw(win, OUT_Y, ERR_X-4, "OUT:");	
	mvwprintw(win, INT_Y, INT_X-4, "INT:");	
	
	mvwprintw(win, P_Y, P_X, "P:");
	mvwprintw(win, I_Y, I_X, "I:");
	mvwprintw(win, D_Y, D_X, "D:");
	mvwprintw(win, MR_Y, MR_X, "MR:");
	mvwprintw(win, RD_Y, RD_X, "RD:");
	

	mvwprintw(win, ON_Y, ON_X, "ON");
	mvwprintw(win, OFF_Y, OFF_X, "OFF");
	mvwprintw(win, LOCKED_Y, LOCKED_X, "LOCKED");

	// update the screen
	touchwin(win);
	wrefresh(win);
	
	// print some values
	UpdatePanel();
}

