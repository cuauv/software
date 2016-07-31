#ifndef __PID_PANEL_H__
#define __PID_PANEL_H__

#include <curses.h>
#include <string>

using namespace std;

namespace controld2 {
    
    //forward class declaration
    class Display;
    
    class PidPanel {
    public:
        //x and y are the top left coordinates of this box
        //variable is the name of the variable this PID loop is run on (e.g., depth, heading, etc) and
        //should conform to a value of /control/internal/[variable]
        //value_var is the name of the auval variable containing the raw input to the controller
        //desire_var is the name of the desire variable
        PidPanel(int x, int y, const string& variable, const string& value_var, const string& desire_var);
        
        ~PidPanel();
        
        //udpates the panel
        void UpdatePanel();
        
        //handles inputs
        void HandleInput();
        
        //re-draws in the event of a re-draw command
        void RedrawPanel();
        
        //toggles this controller on or off
        void toggle();
        
    private:
        int x, y;
        WINDOW* win;
        string name;
        Display* disp;
    };
};

#endif

