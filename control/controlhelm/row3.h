#ifndef __ROW3_H__
#define __ROW3_H__

#include <curses.h>
#include "libshm/c/shm.h"
#include "displayable.h"

class Row3 : public Displayable {
    public:
        Row3(int x, int y);
        void redraw();
        void update();

        WINDOW* getInputWin();

    private:
        WINDOW *input_win;
        WINDOW *bat_win;
        WINDOW *placeholder_win;
};

#endif
