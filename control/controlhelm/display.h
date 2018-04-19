#ifndef __DISPLAY_H__
#define __DISPLAY_H__

#include <curses.h>
#include <pthread.h>
#include <unistd.h>
#include <vector>
#include "pid_panel.h"
#include "new_pid_panel.h"
#include "row1.h"
#include "row3.h"
#include "locations.h"

#include <libshm/c/shm.h>

enum entry_mode {
    MODE_DEFAULT,
    MODE_HEADING,
    MODE_DEPTH,
    MODE_SPEED,
    MODE_LAT_SPEED,
    MODE_PITCH,
    MODE_ROLL,
    MODE_POSN,
    MODE_POSE,
    MODE_CONTROL,
    MODE_ALL
};

enum control_mode {
    MODE_NONE,
    MODE_HEADING_KP,
    MODE_HEADING_KI,
    MODE_HEADING_KD,
    MODE_DEPTH_KP,
    MODE_DEPTH_KI,
    MODE_DEPTH_KD,
    MODE_PITCH_KP,
    MODE_PITCH_KI,
    MODE_PITCH_KD,
    MODE_ROLL_KP,
    MODE_ROLL_KI,
    MODE_ROLL_KD,
    MODE_VELX_KP,
    MODE_VELX_KI,
    MODE_VELX_KD,
    MODE_VELY_KP,
    MODE_VELY_KI,
    MODE_VELY_KD,
};

class Display {

    public:
        Display();
        ~Display();

        void startUpdates();
        void startInput();

        void redraw();
        void update();

        void handleInput();

        void setEMode();

    private:
        pthread_mutex_t screen_lock;

        void addWidget(Displayable *);
        std::vector<Displayable *> displayables;
        Row3 *bottom_row;

        struct navigation_desires desires;
        struct navigation_settings navigation_settings;
        struct kalman kalman;
        struct motor_desires motorDesires;
        struct settings_control controlSettings;
        struct settings_depth depthSettings;
        struct settings_pitch pitchSettings;
        struct settings_roll rollSettings;
        struct settings_heading headingSettings;
        struct settings_velx velxSettings;
        struct settings_vely velySettings;
        struct control_internal_depth internalDepth;
        struct control_internal_pitch internalPitch;
        struct control_internal_roll internalRoll;
        struct control_internal_heading internalHeading;
        struct control_internal_velx internalVelx;
        struct control_internal_vely internalVely;
        struct switches switches;
        struct control_locked locked;

        int sign;
        int emode;
        int spinmode = 0;
        bool entry_valid = true;

        WINDOW* input_win;

        enum entry_mode mode;
        enum control_mode cmode;	

        //Display a message on the input window
        void inputMessage(const string f, ...);
        void clearMessage();

        void newEntryChar(char c);
        void delEntryChar();
        void resetEntry();
        void processEntry();

        void handleToggle();
        void handleMToggle();
        void handleTToggle();
        void handlePToggle();

        void zeroDesires(bool surface);

        char  mesg[RATE_WIDTH];
        char entry[RATE_WIDTH];
        short entry_indx;

        unsigned char cursor_x, cursor_y;

};

#endif

