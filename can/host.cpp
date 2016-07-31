#include <iostream>

#include <thread>
#include <chrono>

#include <curses.h>
#include <signal.h>
#include <string.h>

#include <libshm/c/shm.h>

#include "lib/Log.h"
#include "Manager.h"
#include "SimpleRegistry.h"
#include "ShmRegistry.h"

static bool shouldEnd = false;
static std::shared_ptr<Registry> reg;
static std::shared_ptr<Manager> mgr;

#define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))

void usage() {
    std::cout << "Usage: host intf [--noshm]" << std::endl
              << "       host intf [--nogui]" << std::endl;
}

WINDOW *dev_win, *in_win, *log_win;

enum IState { NORMAL, DEVICE, SETPOINT };
IState in_state = NORMAL;
std::string in_buf;
int selected_device = 0;

void handle_input() {
    try {
        int val = std::stoi(in_buf);
        switch (in_state) {
            case DEVICE:
                selected_device = val;
                break;
            case SETPOINT:
                mgr->setSetpoint(selected_device, val);
                break;
            default:
                break;
        }
    } catch (std::exception& e) {
        Log::log(e.what());
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

    dev_win = newwin(15,80,0,0);
    in_win = newwin(3,80,15,0);
    log_win = newwin(12,80,18,0);
}

void curses_shutdown() {
    delwin(dev_win);
    delwin(in_win);
    delwin(log_win);

    endwin();
}

void curses_update() {
    // Dev win
    werase(dev_win);
    box(dev_win, 0, 0);
    // Header row
    mvwaddstr(dev_win, 1, 1, "DID");
    mvwaddstr(dev_win, 1, 5, "Setpnt");
    mvwaddstr(dev_win, 1, 12, "Feedback0");
    mvwaddstr(dev_win, 1, 22, "Feedback1");
    mvwaddstr(dev_win, 1, 32, "Feedback2");
    mvwaddstr(dev_win, 1, 42, "Feedback3");
    mvwaddstr(dev_win, 1, 52, "Grp Off");
    mvwaddstr(dev_win, 1, 60, "EN");
    mvwaddstr(dev_win, 1, 63, "HID");
    mvwaddstr(dev_win, 1, 67, "FWV");
    int row = 2;
    reg->forEachDevice([&] (std::shared_ptr<Device> dev) {
            if (dev->getDevID() == selected_device) {
                wattron(dev_win, A_STANDOUT);
                mvwprintw(dev_win, row, 1, "%-2u", dev->getDevID());
                wattroff(dev_win, A_STANDOUT);
            } else {
                mvwprintw(dev_win, row, 1, "%-2u", dev->getDevID());
            }
            auto grp = reg->getGroup(dev->getGroup());
            if (grp) {
                mvwprintw(dev_win, row, 5, "%- 5i", grp->getSetpoint(dev->getOffset()));
            }
            mvwprintw(dev_win, row, 12, "%- 5i", dev->getFeedback(0));
            mvwprintw(dev_win, row, 22, "%- 5i", dev->getFeedback(1));
            mvwprintw(dev_win, row, 32, "%- 5i", dev->getFeedback(2));
            mvwprintw(dev_win, row, 42, "%- 5i", dev->getFeedback(3));
            mvwprintw(dev_win, row, 52, "%-2u  %-2u", dev->getGroup(), dev->getOffset());
            if (dev->getHardEnable()) {
                mvwaddstr(dev_win, row, 60, "EN");
            } else {
                wattron(dev_win, A_STANDOUT);
                mvwaddstr(dev_win, row, 60, "DS");
                wattroff(dev_win, A_STANDOUT);
            }
            mvwprintw(dev_win, row, 63, "%-2u", dev->getHWType());
            mvwprintw(dev_win, row, 67, "%-2u", dev->getFWVer());
            row++;
        });
    wnoutrefresh(dev_win);

    werase(in_win);
    box(in_win, 0, 0);

    if (mgr->getSoftEnable()) {
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

    // Log win
    werase(log_win);
    box(log_win, 0, 0);
    row = 10;
    for (auto msg : Log::getMessages()) {
        mvwaddstr(log_win, row--, 1, msg.c_str());
    }
    wnoutrefresh(log_win);

    doupdate();
}

void signal_exit(int UNUSED(signum)) {
    shouldEnd = true;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        usage();
        return -1;
    }

    shm_init();

    Log::init(10,78);

    signal(SIGTERM, signal_exit);
    signal(SIGINT, signal_exit);

    if (argc == 3 && strcmp(argv[2], "--noshm") == 0) {
        reg = std::make_shared<SimpleRegistry>();
    } else {
        reg = std::make_shared<ShmRegistry>();
    }
    mgr = std::make_shared<Manager>(argv[1], reg);
    mgr->init();

    if (argc == 3 && strcmp(argv[2], "--nogui") != 0) {
        curses_init();

        while (!shouldEnd) {
            int ch;
            while ((ch = getch()) != ERR) {
                if (in_state == NORMAL) {
                    switch (ch) {
                        case ' ':
                            mgr->setSoftEnable(false);
                            break;
                        case KEY_F(5):
                            mgr->setSoftEnable(true);
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

        std::cout << "Exited display loop." << std::endl;
    } else {
        while (!shouldEnd) {
            std::this_thread::sleep_for (std::chrono::seconds(2));
        }
    }

    std::cout << "Shutting down manager." << std::endl;

    mgr->shutdown();
    mgr = nullptr;

    return 0;
}
