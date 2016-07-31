#include "common-include.h"
#include <string>
#include <sstream>

using namespace std;

/* Function to determine status of a descending variable (lower values bad) */
int dcomp(double val, double warn, double err) {
    if (val <= err) return ATTR_ERR;
    if (val <= warn) return ATTR_WARN;
    else return ATTR_OK;
}


/* Function to determine status of an ascending variable (higher values bad) */
int acomp(double val, double warn, double err) {
    if (val >= err) return ATTR_ERR;
    if (val >= warn) return ATTR_WARN;
    else return ATTR_OK;
}

/* Function to determine status of a ranged variable */
int rcomp(double val, double warn_low, double err_low, double warn_high, double err_high) {
    if (val <= err_low || val >= err_high) return ATTR_ERR;
    if (val <= warn_low || val >=warn_high) return ATTR_WARN;
    else return ATTR_OK;
}


/* Draws a progress bar in a specific curses window */
void draw_progress_bar(WINDOW *w, int y, int x, int width, int percent, int attr) {
    int len = percent / (100 / width);
    len = (len < 0) ? 0 : len;
    len = (len > width) ? width : len;
    
    if  (percent > 99) {
        len -= 1; // fix drawing error
    }
    
    stringstream ss;
    basic_string <char> pstr(len, ' ');
    ss << pstr << percent << "%%"; // hooray for dumb-ass c++
    basic_string <char>blank(width+4,' ');

    wattrset(w,0);
    mvwprintw(w, y, x, blank.c_str()); // Clears the area so no ghosting
    mvwprintw(w, y, x, "[");
    wattrset(w,attr);
    mvwprintw(w, y, x+1, ss.str().c_str());
    wattrset(w,0);
    mvwprintw(w, y, x+width+4, "]");
}

