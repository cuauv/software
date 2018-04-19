#include <iostream>

#include <thread>
#include <chrono>

#include <curses.h>
#include <signal.h>
#include <string.h>

#include <libshm/c/shm.h>

#include "ShmDevices.h"

static bool shouldEnd = false;
static ShmDeviceList devices;

#define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))

WINDOW *thrust_win, *vector_win, *in_win;

enum IState { NORMAL, DEVICE, SETPOINT };
IState in_state = NORMAL;
std::string in_buf;
int selected_device = 0;

bool getSoftEnable() {
    int kill;
    shm_get(switches, soft_kill, kill);
    return !kill;
}

void setSoftEnable(bool enable) {
    shm_set(switches, soft_kill, !enable);
}

void handle_input() {
    try {
        int val = std::stoi(in_buf);
        switch (in_state) {
            case DEVICE:
                selected_device = val;
                break;
            case SETPOINT:
                devices.setSetpoint(selected_device, val);
                break;
            default:
                break;
        }
    } catch (std::exception& e) {
    }
    in_buf.clear();
}

void curses_init() {
    initscr();
    cbreak();
    noecho();
    nonl();
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);

    thrust_win = newwin(12,80,0,0);
    vector_win = newwin(6,80,12,0);
    in_win = newwin(3,80,18,0);
}

void curses_shutdown() {
    delwin(thrust_win);
    delwin(vector_win);
    delwin(in_win);

    endwin();
}

void curses_update() {
    werase(thrust_win);
    box(thrust_win, 0, 0);

    auto thrusters = devices.getThrusters();
    auto thrust_head = thrusters.front();
    // Header row
    mvwaddstr(thrust_win, 1, 1, "DID");
    mvwaddstr(thrust_win, 1, 5, "Thruster Name");
    mvwaddstr(thrust_win, 1, 20, "Setpoint");
    auto str = thrust_head->getFeedbackName(0);
    if (str == "") {
        mvwaddstr(thrust_win, 1, 30, "Feedback0");
    } else {
        mvwaddnstr(thrust_win, 1, 30, str.c_str(), 11);
    }
    str = thrust_head->getFeedbackName(1);
    if (str == "") {
        mvwaddstr(thrust_win, 1, 42, "Feedback1");
    } else {
        mvwaddnstr(thrust_win, 1, 42, str.c_str(), 11);
    }
    str = thrust_head->getFeedbackName(2);
    if (str == "") {
        mvwaddstr(thrust_win, 1, 54, "Feedback2");
    } else {
        mvwaddnstr(thrust_win, 1, 54, str.c_str(), 11);
    }
    str = thrust_head->getFeedbackName(3);
    if (str == "") {
        mvwaddstr(thrust_win, 1, 66, "Feedback3");
    } else {
        mvwaddnstr(thrust_win, 1, 66, str.c_str(), 11);
    }

    int row = 2;
    for (auto dev : thrusters) {
        if (dev->getStatus()) {
            if (dev->getDevID() == selected_device) {
                wattron(thrust_win, A_STANDOUT);
                mvwprintw(thrust_win, row, 1, "%-2u", dev->getDevID());
                wattroff(thrust_win, A_STANDOUT);
            } else {
                mvwprintw(thrust_win, row, 1, "%-2u", dev->getDevID());
            }

            mvwaddstr(thrust_win, row, 5, dev->getName().c_str());

            if (dev->getStatus() == 1) {
                mvwprintw(thrust_win, row, 20, "%- 5i", dev->getSetpoint());
            }

            mvwprintw(thrust_win, row, 30, "%- 5i", dev->getFeedback(0));
            mvwprintw(thrust_win, row, 42, "%- 5i", dev->getFeedback(1));
            mvwprintw(thrust_win, row, 54, "%- 5i", dev->getFeedback(2));
            mvwprintw(thrust_win, row, 66, "%- 5i", dev->getFeedback(3));
            row++;
        }
    }

    wnoutrefresh(thrust_win);

    werase(vector_win);
    box(vector_win, 0, 0);

    auto vectors = devices.getVectors();
    auto vector_head = vectors.front();
    // Header row
    mvwaddstr(vector_win, 1, 1, "DID");
    mvwaddstr(vector_win, 1, 5, "Vector Name");
    mvwaddstr(vector_win, 1, 20, "Setpoint");
    str = vector_head->getFeedbackName(0);
    if (str == "") {
        mvwaddstr(vector_win, 1, 30, "Feedback0");
    } else {
        mvwaddnstr(vector_win, 1, 30, str.c_str(), 11);
    }
    str = vector_head->getFeedbackName(1);
    if (str == "") {
        mvwaddstr(vector_win, 1, 42, "Feedback1");
    } else {
        mvwaddnstr(vector_win, 1, 42, str.c_str(), 11);
    }
    str = vector_head->getFeedbackName(2);
    if (str == "") {
        mvwaddstr(vector_win, 1, 54, "Feedback2");
    } else {
        mvwaddnstr(vector_win, 1, 54, str.c_str(), 11);
    }
    str = vector_head->getFeedbackName(3);
    if (str == "") {
        mvwaddstr(vector_win, 1, 66, "Feedback3");
    } else {
        mvwaddnstr(vector_win, 1, 66, str.c_str(), 11);
    }

    row = 2;
    for (auto dev : vectors) {
        if (dev->getStatus()) {
            if (dev->getDevID() == selected_device) {
                wattron(vector_win, A_STANDOUT);
                mvwprintw(vector_win, row, 1, "%-2u", dev->getDevID());
                wattroff(vector_win, A_STANDOUT);
            } else {
                mvwprintw(vector_win, row, 1, "%-2u", dev->getDevID());
            }

            mvwaddstr(vector_win, row, 5, dev->getName().c_str());

            if (dev->getStatus() == 1) {
                mvwprintw(vector_win, row, 20, "%- 5i", dev->getSetpoint());
            }

            mvwprintw(vector_win, row, 30, "%- 5i", dev->getFeedback(0));
            mvwprintw(vector_win, row, 42, "%- 5i", dev->getFeedback(1));
            mvwprintw(vector_win, row, 54, "%- 5i", dev->getFeedback(2));
            mvwprintw(vector_win, row, 66, "%- 5i", dev->getFeedback(3));
            row++;
        }
    }

    wnoutrefresh(vector_win);

    werase(in_win);
    box(in_win, 0, 0);

    if (getSoftEnable()) {
        mvwprintw(in_win, 1, 1, "EN");
    } else {
        wattron(in_win, A_STANDOUT);
        mvwprintw(in_win, 1, 1, "DS");
        wattroff(in_win, A_STANDOUT);
    }
    switch (in_state) {
        case NORMAL:
            break;
        case DEVICE:
            wprintw(in_win, " Device: ");
            break;
        case SETPOINT:
            wprintw(in_win, " Setpoint: ");
            break;
    }
    wprintw(in_win, in_buf.c_str());
    mvwprintw(in_win, 1, 60, "Selected Device: %d", selected_device);

    wnoutrefresh(in_win);

    doupdate();
}

void signal_exit(int UNUSED(signum)) {
    shouldEnd = true;
}

int main(int argc, char *argv[]) {
    shm_init();

    signal(SIGTERM, signal_exit);
    signal(SIGINT, signal_exit);

    curses_init();

    while (!shouldEnd) {
        int ch;
        while ((ch = getch()) != ERR) {
            if (in_state == NORMAL) {
                switch (ch) {
                    case ' ':
                        setSoftEnable(false);
                        break;
                    case KEY_F(5):
                        setSoftEnable(true);
                        break;
                    case 'd':
                        in_state = DEVICE;
                        break;
                    case 's':
                        in_state = SETPOINT;
                        break;
                }
            } else {
                if (ch == KEY_ENTER || ch == '\r' || ch == '\n') {
                    handle_input();
                    in_state = NORMAL;
                } else if (ch == 27) { // Escape key
                    in_buf.clear();
                    in_state = NORMAL;
                } else if (ch == KEY_BACKSPACE || ch == 127) {
                    if (!in_buf.empty()) in_buf.pop_back();
                } else {
                    in_buf.push_back(ch);
                }
            }
        }

        curses_update();
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    curses_shutdown();

    std::cout << "Exited cleanly" << std::endl;

    return 0;
}
