#include "display.h"

#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <stdarg.h>

#include "locations.h"

#include "libshm/c/vars.h"

#define FILL_CVP(lcName, rateName, ucName) { \
    cvp.value = &kalman.lcName; \
    cvp.rate = &kalman.rateName; \
    cvp.active = &controlSettings.lcName ## _active; \
    cvp.locked = &locked.lcName; \
    cvp.integral = &internal ## ucName.integral; \
    cvp.out = &internal ## ucName.out; \
    cvp.kP = &lcName ## Settings.kP; \
    cvp.kI = &lcName ## Settings.kI; \
    cvp.kD = &lcName ## Settings.kD; \
    cvp.rD = &lcName ## Settings.rD; \
}

char reverse_keys[] = {')','!','@','#','$','%','^','&','*','('};

Display::Display()
{
    addWidget(new Row1(0, 0));
    Row3* bottom_row = new Row3(0, ROW1_HEIGHT + ROW2_HEIGHT + WIN_HEIGHT + 6);
    addWidget(bottom_row);

    ControlVarPointers cvp;
    // Second row
    FILL_CVP(heading, heading_rate, Heading)
    cvp.desiredValue = &desires.heading;
    addWidget(new NewPidPanel(0, ROW1_HEIGHT+2, "heading", &cvp));

    FILL_CVP(pitch, pitch_rate, Pitch)
    cvp.desiredValue = &desires.pitch;
    addWidget(new NewPidPanel(WIN_WIDTH+2, ROW1_HEIGHT+2,"pitch", &cvp));

    FILL_CVP(roll, roll_rate, Roll)
    cvp.desiredValue = &desires.roll;
    addWidget(new NewPidPanel(2*(WIN_WIDTH+2), ROW1_HEIGHT+2, "roll", &cvp));

    // Third row
    FILL_CVP(velx, accelx, Velx)
    cvp.desiredValue = &desires.speed;
    addWidget(new NewPidPanel(0, ROW1_HEIGHT+ROW2_HEIGHT+4, "velx", &cvp));

    FILL_CVP(vely, accely, Vely)
    cvp.desiredValue = &desires.sway_speed;
    addWidget(new NewPidPanel(WIN_WIDTH+2, ROW1_HEIGHT+ROW2_HEIGHT+4, "vely", &cvp));

    FILL_CVP(depth, depth_rate, Depth)
    cvp.desiredValue = &desires.depth;
    addWidget(new NewPidPanel(2*(WIN_WIDTH+2), ROW1_HEIGHT+ROW2_HEIGHT+4, "depth", &cvp));

    sign = 1;
    emode = 0;
    spinmode = 0;

    input_win = bottom_row->getInputWin();

    pthread_mutex_init(&screen_lock, NULL);

    mode = MODE_DEFAULT;
    cmode = MODE_NONE;
    cursor_x = INPUT_ENTRY_X;
    cursor_y = INPUT_ENTRY_Y;
    resetEntry();

    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    redraw();
}

Display::~Display() {
    endwin();
    pthread_mutex_destroy(&screen_lock);
    for (size_t i = 0; i < displayables.size(); i++) {
        delete displayables[i];
    }
}

void Display::addWidget(Displayable *widget) {
    displayables.push_back(widget);
}

void Display::redraw() {

    pthread_mutex_lock(&screen_lock);

    for (auto itr = displayables.begin(); itr != displayables.end(); itr++) {
        (*itr)->redraw();
    }

    mvwprintw(input_win, INPUT_MESG_Y, INPUT_MESG_X, mesg);
    mvwprintw(input_win, INPUT_ENTRY_Y, INPUT_ENTRY_X, entry);
    wmove(input_win, cursor_y, cursor_x);
    wrefresh(input_win);

    pthread_mutex_unlock(&screen_lock);
}

void Display::update() {

    shm_getg(navigation_desires, desires);
    shm_getg(kalman, kalman);
    shm_getg(motor_desires, motorDesires);
    shm_getg(settings_control, controlSettings);
    shm_getg(settings_depth, depthSettings);
    shm_getg(settings_velx, velxSettings);
    shm_getg(settings_vely, velySettings);
    shm_getg(control_internal_depth, internalDepth);
    shm_getg(control_internal_velx, internalVelx);
    shm_getg(control_internal_vely, internalVely);
    shm_getg(switches, switches);
    shm_getg(control_locked, locked);

    if (!controlSettings.quat_pid) {
        shm_getg(settings_pitch, pitchSettings);
        shm_getg(settings_roll, rollSettings);
        shm_getg(settings_heading, headingSettings);
        shm_getg(control_internal_roll, internalRoll);
        shm_getg(control_internal_pitch, internalPitch);
        shm_getg(control_internal_heading, internalHeading);
    }
    else {
        // lol
        headingSettings.kP = 999;
        headingSettings.kI = 999;
        headingSettings.kD = 999;
        headingSettings.rD = 999;
        internalHeading.out = 999;
        internalHeading.integral = 999;
        // lol
        pitchSettings.kP = 999;
        pitchSettings.kI = 999;
        pitchSettings.kD = 999;
        pitchSettings.rD = 999;
        internalPitch.out = 999;
        internalPitch.integral = 999;
        // lol
        rollSettings.kP = 999;
        rollSettings.kI = 999;
        rollSettings.kD = 999;
        rollSettings.rD = 999;
        internalRoll.out = 999;
        internalRoll.integral = 999;
    }

    if (spinmode == 1) shm_set(navigation_desires, heading, fmod(kalman.heading + 120, 360));
    if (spinmode == -1) shm_set(navigation_desires, heading, fmod(kalman.heading - 120, 360));

    pthread_mutex_lock(&screen_lock);

    for (auto itr = displayables.begin(); itr != displayables.end(); itr++) {
        (*itr)->update();
    }

    mvwprintw(input_win, INPUT_MESG_Y, INPUT_MESG_X, mesg);
    mvwprintw(input_win, INPUT_ENTRY_Y, INPUT_ENTRY_X, entry);
    wmove(input_win, cursor_y, cursor_x);
    wrefresh(input_win);

    pthread_mutex_unlock(&screen_lock);
}

double max(double a, double b){
    if (a>b)return a;
    else return b;
}

double min(double a, double b){
    if (a<b)return a;
    else return b;
}

void Display::handleInput() {
    int x;
    int t;
    int i;
    char first_default = 1;
    //get the current character and handle it
    nodelay(stdscr, TRUE); // don't block on keyreads
    cbreak();
    noecho();
    keypad(stdscr, TRUE); // let us read keys other than alpha nums

    //get and process up to four characters, taking care to only process arrow keys
    //and stuff once
    for (i=0; i<4 && (x=getch()) != ERR; i++) {
        switch(x){
            case ERR:
                // no key pressed. Do nothing
                break;
            case 27: // escape key
                mode = MODE_DEFAULT;
                resetEntry();
                clearMessage();
                inputMessage("");
                break;
            case ' ': // kill signal is space
                shm_set(switches, soft_kill, 1);
                inputMessage("KILLED");
                break;
            case KEY_F(5):
                if (first_default) {
                    shm_set(switches, soft_kill, 0);
                    inputMessage("UNKILLED");
                    first_default = 0;
                }
                break;
            case KEY_F(12):
                if (first_default) {
                    shm_set_settings_control_enabled(
                            !shm_get_settings_control_enabled());
                    inputMessage("Toggle Controller");
                    first_default = 0;
                }
                break;
            case KEY_UP:
                if (first_default) {
                    shm_set_navigation_desires_depth(shm_get_desires_depth() - 0.1);
                    first_default = 0;
                }
                break;
            case KEY_DOWN:
                if (first_default) {
                    shm_set_navigation_desires_depth(shm_get_desires_depth() + 0.1);
                    first_default = 0;
                }
                break;
            case KEY_SLEFT:
                if (first_default) {
                    shm_set_navigation_desires_sway_speed(shm_get_desires_sway_speed() - 0.1);
                    spinmode = 0;
                    first_default = 0;
                }
                break;
            case KEY_SRIGHT:
                if (first_default) {
                    shm_set_navigation_desires_sway_speed(shm_get_desires_sway_speed() + 0.1);
                    spinmode = 0;
                    first_default = 0;
                }
                break;
            case KEY_LEFT:
                if (first_default) {
                    shm_set_navigation_desires_heading(fmod(shm_get_desires_heading()+355, 360));
                    spinmode = 0;
                    first_default = 0;
                }
                break;
            case KEY_RIGHT:
                if (first_default) {
                    shm_set_navigation_desires_heading(fmod(shm_get_desires_heading()+365, 360));
                    spinmode = 0;
                    first_default = 0;
                }
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case '0':
                t = x - '1';

                if (mode == MODE_DEFAULT) {
                    if (t < 0) {
                        t = 9;
                    }
                    //Scale for new controller
                    shm_set_navigation_desires_speed((t * 0.1));
                    spinmode = 0;
                    inputMessage("");
                } else {
                    newEntryChar(x);
                }
                break;
            case '!':
            case '@':
            case '#':
            case '$':
            case '%':
            case '^':
            case '&':
            case '*':
            case '(':
            case ')':
                //shift + number key for reverse!
                int i;
                for(i = 0; i <= 9; i++) {
                    if (reverse_keys[i] == x) {
                        break;
                    }
                }

                t = i - 1;

                if (mode == MODE_DEFAULT) {
                    if (t < 0) {
                        t = 9;
                    }
                    //Scale for new controller
                    shm_set_navigation_desires_speed((-t * 0.1));
                } else {
                    newEntryChar(x);
                }
                inputMessage("");

                break;
            case '>':
                spinmode = 1;
                inputMessage("Fast Spin CW");
                break;
            case '<':
                spinmode = -1;
                inputMessage("Fast Spin CCW");
                break;
            case '.':
                if (mode != MODE_DEFAULT) {
                    newEntryChar(x);
                }
            case '-':
                if (mode == MODE_DEFAULT && first_default) {
                    //if we are at 0 and slow down, stay at 0
                    double speed;
                    shm_get(desires, speed, speed);
                    if (speed >= 0) {
                        shm_set_navigation_desires_speed((max(speed - 0.01, 0.0)));
                    } else {
                        shm_set_navigation_desires_speed(min(speed + 0.01, 0.0));
                    }
                    first_default = 0;
                }
                if (mode == MODE_SPEED || mode == MODE_LAT_SPEED || 
                    mode == MODE_PITCH || mode == MODE_ROLL ||
                    mode == MODE_POSN  || mode == MODE_POSE) {
                    newEntryChar(x);
                }
                break;
            case '=':
                if(first_default) {
                    // if we would go faster than 1, don't
                    double speed;
                    shm_get(desires, speed, speed);
                    if (speed >= 0.0) {
                        shm_set_navigation_desires_speed(min(speed + 0.01, 1.0));
                    } else {
                        shm_set_navigation_desires_speed(max(speed - 0.01, -1.0));
                    }
                    first_default = 0;
                }
                break;
            case KEY_BACKSPACE:
            case 127: // The other backspace.
                if (mode == MODE_DEFAULT) {
                    //sign=sign*-1;
                    //shm_set_navigation_desires_speed(-shm_get_desires_speed());
                    //disable the horror that is backspace
                } else {
                    delEntryChar();
                }
                break;
      case 'a':
          mode = MODE_ALL;
          break;
      case 'H':
            case 'h':
                mode = MODE_HEADING;
                inputMessage("Enter Heading:");
                break;
            case 'P':
            case 'p':
                //able to change control constants
                if(emode == 1 && (mode==MODE_HEADING || mode==MODE_PITCH || mode==MODE_DEPTH || mode==MODE_SPEED || mode==MODE_LAT_SPEED || mode==MODE_ROLL)){
                    switch(mode){
                        case MODE_DEFAULT:
                        case MODE_CONTROL:
                        case MODE_POSE:
                        case MODE_POSN:
                        case MODE_ALL:
                            return;
                            break;
                        case MODE_HEADING:
                            cmode = MODE_HEADING_KP;
                            break;
                        case MODE_DEPTH:
                            cmode = MODE_DEPTH_KP;
                            break;
                        case MODE_PITCH:
                            cmode = MODE_PITCH_KP;
                            break;
                        case MODE_ROLL:
                            cmode = MODE_ROLL_KP;
                            break;
                        case MODE_SPEED:
                            cmode = MODE_VELX_KP;
                            break;
                        case MODE_LAT_SPEED:
                            cmode = MODE_VELY_KP;
                            break;
                    }
                    mode = MODE_CONTROL;
                    inputMessage("Enter KP:");
                } else {
                    mode = MODE_PITCH;
                    inputMessage("Enter Pitch:");
                }
                break;
            case 'D':
            case 'd':
                //able to change control constants
                if(emode == 1 && (mode==MODE_HEADING || mode==MODE_PITCH || mode==MODE_DEPTH || mode==MODE_SPEED || mode==MODE_LAT_SPEED || mode==MODE_ROLL)){
                    switch(mode){
                        case MODE_DEFAULT:
                        case MODE_CONTROL:
                        case MODE_POSE:
                        case MODE_POSN:
                        case MODE_ALL:
                            return;
                            break;
                        case MODE_HEADING:
                            cmode = MODE_HEADING_KD;
                            break;
                        case MODE_DEPTH:
                            cmode = MODE_DEPTH_KD;
                            break;
                        case MODE_PITCH:
                            cmode = MODE_PITCH_KD;
                            break;
                        case MODE_ROLL:
                            cmode = MODE_ROLL_KD;
                            break;
                        case MODE_SPEED:
                            cmode = MODE_VELX_KD;
                            break;
                        case MODE_LAT_SPEED:
                            cmode = MODE_VELY_KD;
                            break;
                    }
                    mode = MODE_CONTROL;
                    inputMessage("Enter KD:");
                } else {
                    mode = MODE_DEPTH;
                    inputMessage("Enter Depth:");
                }
                break;
            case 'R':
            case 'r':
                mode = MODE_ROLL;
                inputMessage("Enter Roll:");
                break;
            case 'I':
            case 'i':
                //able to change control constants
                if(emode == 1 && (mode==MODE_HEADING || mode==MODE_PITCH || mode==MODE_DEPTH || mode==MODE_SPEED || mode==MODE_LAT_SPEED || mode==MODE_ROLL)){
                    switch(mode){
                        case MODE_DEFAULT:
                        case MODE_CONTROL:
                        case MODE_POSE:
                        case MODE_POSN:
                        case MODE_ALL:
                            return;
                            break;
                        case MODE_HEADING:
                            cmode = MODE_HEADING_KI;
                            break;
                        case MODE_DEPTH:
                            cmode = MODE_DEPTH_KI;
                            break;
                        case MODE_PITCH:
                            cmode = MODE_PITCH_KI;
                            break;
                        case MODE_ROLL:
                            cmode = MODE_ROLL_KI;
                            break;
                        case MODE_SPEED:
                            cmode = MODE_VELX_KI;
                            break;
                        case MODE_LAT_SPEED:
                            cmode = MODE_VELY_KI;
                            break;
                    }
                    inputMessage("Enter KI:");
                    mode = MODE_CONTROL;
                }
                break;
            case 'U':
            case 'u':
                if (first_default) {
                    shm_set_navigation_desires_heading(fmod(shm_get_desires_heading()+180, 360));
                    first_default = 0;
                }
                break;
            case 'Z':
                zeroDesires(true /*surface*/);
                inputMessage("Surface!");
                spinmode = 0;
                break;
            case 'z':
                zeroDesires(false /*surface*/);
                inputMessage("Zero movements");
                spinmode = 0;
                break;
            case 'X':
            case 'x':
                mode = MODE_SPEED;
                inputMessage("Enter Vel_X:");
                break;
            case 'Y':
            case 'y':
                mode = MODE_LAT_SPEED;
                inputMessage("Enter Vel_Y:");
                break;

            case 'l':
                mode = MODE_POSN;
                inputMessage("Enter north position:");
                break;

            case KEY_ENTER:
            case '\n':
            case '\r':
                processEntry();
                //FALL-THROUGH
            case 'c':  //cancel
                if (mode == MODE_POSN) {
                    resetEntry();
                    mode = MODE_POSE;
                    clearMessage();
                    inputMessage("Enter east position:");
                    break;
                }

                if (mode != MODE_DEFAULT) {
                    if (not entry_valid) {
                        resetEntry();
                        inputMessage("Invalid; try again");
                    } else {
                        resetEntry();
                        mode = MODE_DEFAULT;
                        clearMessage();
                    }
                }
                break;

            case 'o': //toggle the controller
                handleToggle();
                if (mode != MODE_DEFAULT) {
                    resetEntry();
                    mode = MODE_DEFAULT;
                    clearMessage();
                }
                break;
            case 'm': //toggle mission start or control mode
                handleMToggle();
                if (mode != MODE_DEFAULT) {
                    resetEntry();
                    mode = MODE_DEFAULT;
                    clearMessage();
                }
                break;

            case 'n':
                handlePToggle();
                if (mode != MODE_DEFAULT) {
                    resetEntry();
                    mode = MODE_DEFAULT;
                    clearMessage();
                }
                break;

            case 't':
                handleTToggle();
                if (mode != MODE_DEFAULT) {
                    resetEntry();
                    mode = MODE_DEFAULT;
                    clearMessage();
                }
                break;

            //case 'F': //FIRE
            //    shm_set(actuator_3, trigger, 1);
            //    inputMessage("FIRE");
            //    break;
            // Whoa danger above - reenable later - Alex S
            case 'T':
                inputMessage("NOT IMPLEMENTED");
                break;
            case 'q':
                // DISABLED: this is too easy to accidently hit
                // it's effectively an "oops I just messed up controls" button
                //  REENABLED by Alex
                bool quat = shm_get_settings_control_quat_pid();
                shm_set_settings_control_quat_pid(not quat);

                if (not quat) {
                    inputMessage("Quaternion Mode");
                } else {
                    inputMessage("Euler Angle Mode");
                }

                break;
        }
    }
    flushinp();// trying to prevent delayed input from filled buffer
}

void Display::handleToggle() {
    struct settings_control rwSettings;
    shm_getg(settings_control, rwSettings);
    switch(mode) {
        case MODE_DEFAULT:
        case MODE_POSE:
        case MODE_POSN:
        case MODE_CONTROL:
            return;
            break;
        case MODE_HEADING:
            rwSettings.heading_active = !rwSettings.heading_active;
            break;

        case MODE_DEPTH:
            rwSettings.depth_active = !rwSettings.depth_active;
            break;

        case MODE_SPEED:
            rwSettings.velx_active = !rwSettings.velx_active;
            break;

        case MODE_LAT_SPEED:
            rwSettings.vely_active = !rwSettings.vely_active;
            break;

        case MODE_PITCH:
            rwSettings.pitch_active = !rwSettings.pitch_active;
            break;

        case MODE_ROLL:
            rwSettings.roll_active = !rwSettings.roll_active;
            break;

        case MODE_ALL:
            int val = !rwSettings.heading_active;
            rwSettings.heading_active = val;
            rwSettings.pitch_active = val;
            rwSettings.depth_active = val;
            rwSettings.velx_active = val;
            rwSettings.vely_active = val;
            rwSettings.roll_active = val;
            break;
    }
    shm_setg(settings_control, rwSettings);
}


void Display::handleMToggle() {
    if (mode == MODE_DEFAULT) {
        shm_set_mission_start_switch_mission_start(!shm_get_mission_start_switch_mission_start());
        inputMessage("Mission Start");
    } else if (mode == MODE_CONTROL || mode == MODE_ALL) {
        return;
    } else {
        return;
    }
}

void Display::handleTToggle() {
        shm_set_navigation_settings_optimize(!shm_get_navigation_settings_optimize());
    if (mode == MODE_DEFAULT) {
        inputMessage("Trajectories Toggled");
    } else if (mode == MODE_CONTROL || mode == MODE_ALL) {
        return;
    } else {
        return;
    }
}

void Display::handlePToggle() {
    if (mode == MODE_DEFAULT) {
        shm_set_navigation_settings_position_controls(!shm_get_navigation_settings_position_controls());
        inputMessage("PosCon Toggled");
    } else if (mode == MODE_CONTROL || mode == MODE_ALL) {
        return;
    } else {
        return;
    }
}

void Display::processEntry() {
    spinmode = 0;
    double entry_value = atof(entry);
    switch (mode) {
        case MODE_DEFAULT:
        case MODE_ALL:
            return;
            break;

        case MODE_HEADING:
            if (entry_value > 360 || entry_value < 0) {
                entry_valid = false;
            } else {
                entry_valid = true;
                shm_set(navigation_desires, heading, entry_value);
            }
            break;

        case MODE_POSN:
            shm_set(navigation_desires, north, entry_value);
            break;

        case MODE_POSE:
            shm_set(navigation_desires, east, entry_value);
            break;

        case MODE_DEPTH:
            if (entry_value > 3 || entry_value < -.2) {
                entry_valid = false;
            } else {
                entry_valid = true;
                shm_set(navigation_desires, depth, entry_value);
            }
            break;

        case MODE_SPEED:
            if (entry_value > 10 || entry_value < -10) {
                entry_valid = false;
            } else {
                entry_valid = true;
                shm_set_navigation_desires_speed(entry_value*0.1); //Scale for new controller
            }
            break;

        case MODE_LAT_SPEED:
            if (entry_value > 10 || entry_value < -10) {
                entry_valid = false;
            } else {
                entry_valid = true;
                shm_set(navigation_desires, sway_speed, entry_value*0.1); //Scale for new controller
            }
            break;

        case MODE_PITCH:
           if ((!controlSettings.quat_pid && (entry_value > 70.0 || entry_value < -70.0)) 
                || (entry_value > 90.0 || entry_value < -90.0)) {
               entry_valid = false; 

           } else {
               entry_valid = true;
               shm_set(navigation_desires, pitch, entry_value);
           }
           break;

        case MODE_ROLL:
            if (entry_value > 180 || entry_value < -180) {
                entry_valid = false;
            } else {
                entry_valid = true;
                shm_set(navigation_desires, roll, entry_value);
            }
            break;

        case MODE_CONTROL:
            if (emode == 1) {
                switch (cmode) {
                    case MODE_NONE:
                       return;
                       break;
                    case MODE_HEADING_KP:
                       shm_set(settings_heading, kP, entry_value)
                       break;
                    case MODE_HEADING_KI:
                       shm_set(settings_heading, kI, entry_value)
                       break;
                    case MODE_HEADING_KD:
                       shm_set(settings_heading, kD, entry_value)
                       break;
                    case MODE_DEPTH_KP:
                       shm_set(settings_depth, kP, entry_value)
                       break;
                    case MODE_DEPTH_KI:
                       shm_set(settings_depth, kI, entry_value)
                       break;
                    case MODE_DEPTH_KD:
                       shm_set(settings_depth, kD, entry_value)
                       break;
                    case MODE_PITCH_KP:
                       shm_set(settings_pitch, kP, entry_value)
                       break;
                    case MODE_PITCH_KI:
                       shm_set(settings_pitch, kI, entry_value)
                       break;
                    case MODE_PITCH_KD:
                       shm_set(settings_pitch, kD, entry_value)
                       break;
                    case MODE_ROLL_KP:
                       shm_set(settings_roll, kP, entry_value)
                       break;
                    case MODE_ROLL_KD:
                       shm_set(settings_roll, kD, entry_value)
                       break;
                    case MODE_ROLL_KI:
                       shm_set(settings_roll, kI, entry_value)
                       break;
                    case MODE_VELX_KP:
                       shm_set(settings_velx, kP, entry_value)
                       break;
                    case MODE_VELX_KI:
                       shm_set(settings_velx, kI, entry_value)
                       break;
                    case MODE_VELX_KD:
                       shm_set(settings_velx, kD, entry_value)
                       break;
                    case MODE_VELY_KP:
                       shm_set(settings_vely, kP, entry_value)
                       break;
                    case MODE_VELY_KI:
                       shm_set(settings_vely, kI, entry_value)
                       break;
                    case MODE_VELY_KD:
                       shm_set(settings_vely, kD, entry_value)
                       break;
               }
               cmode = MODE_NONE;
           } else {
               inputMessage("Access Denied");
           }
    }
}


void Display::inputMessage(const string f, ...) {
    const char* fmt = f.c_str();
    va_list ap;

    va_start(ap, f);
    vsnprintf(mesg, RATE_WIDTH, fmt, ap);
    va_end(ap);

    pthread_mutex_lock(&screen_lock);

    mvwprintw(input_win, INPUT_MESG_Y, INPUT_MESG_X, mesg);
    wmove(input_win, cursor_y, cursor_x);

    wrefresh(input_win);

    pthread_mutex_unlock(&screen_lock);

}

void Display::clearMessage() {
    memset(mesg, ' ', RATE_WIDTH-1);
    mesg[RATE_WIDTH-1] = 0;

    pthread_mutex_lock(&screen_lock);

    mvwprintw(input_win, INPUT_MESG_Y, INPUT_MESG_X, mesg);
    wmove(input_win, cursor_y, cursor_x);

    wrefresh(input_win);

    pthread_mutex_unlock(&screen_lock);
}

void Display::newEntryChar(char c) {
    if (entry_indx < RATE_WIDTH) {
        entry[entry_indx] = c;
        entry_indx++;
        cursor_x++;
    }
}

void Display::delEntryChar() {
    if (entry_indx >= 0) {
        entry[entry_indx] = ' ';
        entry_indx--;
        cursor_x--;
    }
}

void Display::resetEntry() {
    memset(entry, ' ', RATE_WIDTH-1);
    entry[RATE_WIDTH-1] = 0;
    entry_indx = 0;
    cursor_x = INPUT_ENTRY_X;
}

void Display::startUpdates() {
    int i;

    redraw();
    usleep(200000);
    for (i=0; i<3; i++) {
        update();
        usleep(200000);
    }
}

void Display::setEMode() {
    emode = 1;
    // Change Window Color
    assume_default_colors(COLOR_WHITE,COLOR_BLUE);
    start_color();
    init_pair(1, COLOR_RED, COLOR_BLUE);
    inputMessage("EXPERT MODE");
}

void Display::zeroDesires(bool surface) {
    struct navigation_desires rw_desires;
    shm_getg(navigation_desires, rw_desires);
    shm_get(kalman, heading, rw_desires.heading);
    shm_get(kalman, north, rw_desires.north);
    shm_get(kalman, east, rw_desires.east);
    if (surface) {
        rw_desires.depth = 0;
    } else {
        rw_desires.depth = shm_get_kalman_depth();
    }
    rw_desires.speed = 0;
    rw_desires.sway_speed = 0;
    rw_desires.pitch = 0;
    rw_desires.roll = 0;
    shm_setg(navigation_desires, rw_desires);
}

