#include <iostream>
#include <exception>
#include <chrono>
#include <thread>

#include <curses.h>
#include <signal.h>

#include "FakeDev.h"
#include <can/lib/CANSocket.h>
#include <can/lib/Log.h>

static bool shouldEnd = false;
static FakeDev* dev;

#define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))

void usage() {
    std::cout << "Usage: vdev intf devid" << std::endl;
    std::cout << "  devid must be greater than 0" << std::endl;
}

void deviceLoop(const char *ifname) {
    auto last_msg = std::chrono::steady_clock::now();

    try {
        CANSocket sock(ifname);
        while (!shouldEnd) {
            struct timeval timeout;
            timeout.tv_sec = 0;
            timeout.tv_usec = 250000;

            try {
                auto msg = sock.readMessage(&timeout);
                if (msg) {
                    if (dev->wantsMessage(*msg)) {
                        last_msg = std::chrono::steady_clock::now();
                        auto reply = dev->handleMessage(*msg);
                        if (reply) {
                            sock.sendMessage(*reply);
                        }
                    }
                }
            } catch (std::exception& e) {
                Log::log("[WARN] " + std::string(e.what()));
            }

            if (dev->getGroup() != 0 && std::chrono::steady_clock::now() > last_msg + std::chrono::milliseconds(500)) {
                Log::log("Host Timed Out");
                dev->setGroup(0);
            }
        }
    } catch (std::exception& e) {
        Log::log("[ERR] " + std::string(e.what()));
    }
}

WINDOW *kill_win, *log_win, *status_win;

void curses_init() {
    initscr();
    cbreak();
    noecho();
    nonl();
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);

    kill_win = newwin(4, 12, 0, 0);
    log_win = newwin(12, 43, 4, 0);
    status_win = newwin(4, 30, 0, 13);
}

void curses_shutdown() {
    delwin(kill_win);
    delwin(log_win);
    delwin(status_win);

    endwin();
}

void curses_update() {
    // Kill win
    box(kill_win, 0, 0);
    if (dev->getSEnable()) {
        mvwaddstr(kill_win, 1, 2, "S ENABLE");
    } else {
        wattron(kill_win, A_STANDOUT);
        mvwaddstr(kill_win, 1, 2, "S KILLED");
        wattroff(kill_win, A_STANDOUT);
    }
    if (dev->getHEnable()) {
        mvwaddstr(kill_win, 2, 2, "H ENABLE");
    } else {
        wattron(kill_win, A_STANDOUT);
        mvwaddstr(kill_win, 2, 2, "H KILLED");
        wattroff(kill_win, A_STANDOUT);
    }
    wnoutrefresh(kill_win);

    // Log win
    werase(log_win);
    box(log_win, 0, 0);
    int row = 10;
    for (auto msg : Log::getMessages()) {
        mvwaddstr(log_win, row--, 1, msg.c_str());
    }
    wnoutrefresh(log_win);

    // Status win
    werase(status_win);
    box(status_win, 0, 0);
    mvwprintw(status_win, 1, 2, "DevID: %-2u", dev->getDevID());
    if (dev->getGroup() == 0) {
        wattron(status_win, A_STANDOUT);
        mvwaddstr(status_win, 2, 2, "Group:");
        wattroff(status_win, A_STANDOUT);
    } else {
        mvwaddstr(status_win, 2, 2, "Group:");
    }
    wprintw(status_win, " %-2u", dev->getGroup());
    mvwprintw(status_win, 1, 12, "Value: %- 5i", dev->getValue());
    mvwprintw(status_win, 2, 12, "Offset: %-2u", dev->getOffset());
    wnoutrefresh(status_win);

    doupdate();
}

void signal_exit(int UNUSED(signum)) {
    shouldEnd = true;
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        usage();
        return -1;
    }

    int devid = atoi(argv[2]);
    if (devid <= 0) {
        usage();
        return -1;
    }

    Log::init(10, 41);

    signal(SIGTERM, signal_exit);
    signal(SIGINT, signal_exit);

    dev = new FakeDev(devid, 1, 1);

    std::thread devThread(deviceLoop, argv[1]);

    curses_init();

    while (!shouldEnd) {
        // Process input
        int ch;
        while ((ch = getch()) != ERR) {
            switch (ch) {
                case ' ':
                    dev->setHEnable(!dev->getHEnable());
                    break;
                default:
                    break;
            }
        }
        // Update Screen
        curses_update();

        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    curses_shutdown();

    devThread.join();

    return 0;
}
