#include "display.h"
#include "battery.h"

#include <curses.h>
#include <pthread.h>
#include <signal.h>

#include <stdio.h>
#include <string.h>

//display update period
#define DISP_PERIOD 75000

void* displayLoop(void * display);
void handle_signal(int signum);

int live=0;

int main(int argc, const char* argv[]) {
	Display* disp;
    pthread_t display;

    shm_init();

	initscr();
	
	disp = new Display();

    signal(SIGTERM, &handle_signal);
    signal(SIGINT, &handle_signal);    

    //check arguments
    for( int i = 0; i < argc; i++ )
    {
        fprintf(stderr, "check mode");
        if( strcmp(argv[i],"-e") == 0 || strcmp(argv[i], "-E") == 0 || strcmp(argv[i], "--EXPERT") == 0 ) {
            disp->setEMode();
            break;
        }
    }

    start_color();
    use_default_colors();
    init_color(COLOR_BLACK, 0, 0, 0);

    //start updating the display
    pthread_create(&display, NULL, &displayLoop, (void*)disp);

    while (live < 2) {
        disp->handleInput();

        usleep(DISP_PERIOD);
    }

    fprintf(stderr, "Quiting Gracefully...\n");

	return 99;
}

void* displayLoop(void* disp) {
    int i;

    //Initial delay hack: seems to prevent ncurses from going insane on startup most of the time/
    usleep(DISP_PERIOD/2);
    
    while (live < 1) {
        // TODO: This is a hack. Ian notes that if this program used ncurses properly, this would
        // not be necessary. It seems like we need to make sure that the strings we output are
        // truncated properly. -- jyc57, 6/3/15
        helm_update_battery();
        ((Display*)disp)->redraw();

        usleep(DISP_PERIOD);
        
        for (i=0; i<9 && live < 1; i++) {
            helm_update_battery();
            ((Display*)disp)->update();
            usleep(DISP_PERIOD);
        }
    }

    delete ((Display*)disp);
    live++;
    fprintf(stderr, "Stopping Display Loop...\n");
    pthread_exit(NULL);
    return NULL;
}

//this may not actually do anything....
void handle_signal(int signum) {
    live = 1;
    fprintf(stderr, "Handling SIGTERM\n");
}

