#ifndef __NEW_PID_PANEL_H__
#define __NEW_PID_PANEL_H__

#include <curses.h>
#include <string>
#include <string.h>
#include <sys/types.h>
#include "displayable.h"
#include "locations.h"

using namespace std;

typedef struct ControlVarPointers {
    double *value;
    double *rate;
    double *desiredValue;
    int32_t *active;
    int32_t *locked;
    double *integral;
    double *out;
    double *kP;
    double *kI;
    double *kD;
    double *rD;
} ControlVarPointers;

//forward class declaration
class Display;

class NewPidPanel : public Displayable {
    public:
        //x and y are the top left coordinates of this box
        //variable is the name of the variable this PID loop is run on (e.g., depth, heading, etc) and
        //should conform to a value of /control/internal/[variable]
        //value_var is the name of the auval variable containing the raw input to the controller
        //desire_var is the name of the desire variable
        NewPidPanel(int x, int y, const string& variable,
                ControlVarPointers *cvp);

        ~NewPidPanel() {};

        //udpates the panel
        void update();

        //handles inputs
        void HandleInput();

        //returns control active
        bool getActive();
        //turn on controller
        void turnOn();
        //turn off controller
        void turnOff();

        //returns control mode
        bool getMode();

        //re-draws in the event of a re-draw command
        void redraw();

        //toggles this controller on or off
        void toggle();

        //toggles this controller mode
        void toggleMode();

    private:
        int x, y;
        WINDOW* win;
        string name;
        Display* disp;

        ControlVarPointers vars;
};

#endif

