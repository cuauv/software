#ifndef HD_COMMON_INCLUDE_H
#define HD_COMMON_INCLUDE_H

#include <curses.h>
#include <unistd.h>
#include <signal.h>


/* The attributes for the various states */
#define ATTR_OK A_STANDOUT | COLOR_PAIR(1)
#define ATTR_WARN A_STANDOUT | COLOR_PAIR(2)
#define ATTR_ERR A_STANDOUT| A_BOLD | COLOR_PAIR(3)
#define ATTR_OVER_100 A_STANDOUT | COLOR_PAIR(4)

/* The constant ranges for all values*/
#include "ranges.h"

// Function to determine status of a descending variable (lower values bad)
extern int dcomp(double val, double warn, double err);


// Function to determine status of an ascending variable (higher values bad)
extern int acomp(double val, double warn, double err);

// Function to determine status of a ranged variable
extern int rcomp(double val, double warn_low, double err_low, double warn_high, double err_high);

/* Draws a progress bar in a specific curses window */
void draw_progress_bar(WINDOW *w, int y, int x, int width, int percent, int attr);

#endif /* HD_COMMON_INCLUDE_H */
