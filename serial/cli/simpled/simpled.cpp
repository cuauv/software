#include <iostream>
#include <fstream>
#include <sstream>
#include <exception>
#include <chrono>

#include <curses.h>
#include <signal.h>

#include <UnixFilePort.h>
#include <Manager.h>

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

static bool shouldEnd = false;
static Manager* manager = nullptr;
static std::shared_ptr<DeviceInfo> devInfo = nullptr;
static std::map<std::string, BoundVariable> readVars;
static std::map<std::string, BoundVariable> writeVars;
static std::map<std::string, BoundVariable>::iterator writeSel;
static bool hardKill = true;
static bool softKill = true;
static bool hardKillValid = false;

static bool got_error = false;
static std::string error_message;

static std::mutex varLock;

static void signal_exit(int UNUSED(signum)) {
    shouldEnd = true;
}

static WINDOW *info_win, *read_win, *write_win;

enum class IState { NORMAL, VALUE };
static IState in_state = IState::NORMAL;
static std::string in_buf;

#define WINDOW_WIDTH 40

static void curses_refresh_windows() {
    if (write_win) delwin(write_win);
    if (read_win) delwin(read_win);
    if (info_win) delwin(info_win);

    clear();

    info_win = newwin(4, WINDOW_WIDTH, 0, 0);
    auto read_win_height = readVars.size() + 3;
    read_win = newwin(read_win_height, WINDOW_WIDTH, 4, 0);
    auto write_win_height = writeVars.size() + 3;
    write_win = newwin(write_win_height, WINDOW_WIDTH, 4 + read_win_height, 0);
}

static void curses_init() {
    initscr();
    cbreak();
    noecho();
    nonl();
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);
    curs_set(0);

    info_win = read_win = write_win = nullptr;

    curses_refresh_windows();
}

static void curses_shutdown() {
    delwin(write_win);
    delwin(read_win);
    delwin(info_win);

    endwin();
}

static void curses_update() {
    werase(info_win);
    box(info_win, 0, 0);
    if (devInfo) {
        mvwprintw(info_win, 1, 2, "Name: %s", devInfo->name().c_str());
        mvwprintw(info_win, 2, 2, "Type: %s", devInfo->type().c_str());
    } else {
        mvwaddstr(info_win, 1, 2, "No Device");
    }

    if (hardKill) {
        wattron(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 1, 28, "HARD");

    if (softKill) {
        wattron(info_win, A_STANDOUT);
    } else {
        wattroff(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 2, 28, "SOFT");

    if (hardKillValid) {
        wattron(info_win, A_STANDOUT);
    } else {
        wattroff(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 1, 33, "VALID");

    if (devInfo) {
        wattron(info_win, A_STANDOUT);
        mvwaddstr(info_win, 2, 33, "CONND");
    } else {
        wattroff(info_win, A_STANDOUT);
        mvwaddstr(info_win, 2, 33, "DCONN");
    }
    wattroff(info_win, A_STANDOUT);
    wnoutrefresh(info_win);

    werase(read_win);
    box(read_win, 0, 0);
    mvwaddstr(read_win, 1, 2, "Name");
    mvwaddstr(read_win, 1, 20, "Value");
    mvwaddstr(read_win, 1, 30, "Reg");
    mvwaddstr(read_win, 1, 34, "Size");
    auto row = 2;
    for (auto var : readVars) {
        mvwaddstr(read_win, row, 2, var.first.c_str());

        if (var.second.type() == Variable::Type::FLOAT) {
            mvwprintw(read_win, row, 20, "%-7f", var.second.getFloat());
        } else {
            mvwprintw(read_win, row, 20, "%-7i", var.second.getInt());
        }
        mvwprintw(read_win, row, 30, "%-3d", var.second.startReg());
        mvwprintw(read_win, row, 34, "%-1d", var.second.size());
        row++;
    }
    wnoutrefresh(read_win);

    werase(write_win);
    box(write_win, 0, 0);
    mvwaddstr(write_win, 1, 2, "Name");
    mvwaddstr(write_win, 1, 20, "Value");
    mvwaddstr(write_win, 1, 30, "Reg");
    mvwaddstr(write_win, 1, 34, "Size");
    row = 2;
    for (auto var : writeVars) {
        if (writeSel != writeVars.end() && *writeSel == var) {
            wattron(write_win, A_STANDOUT);
            mvwaddstr(write_win, row, 2, var.first.c_str());
            wattroff(write_win, A_STANDOUT);
        } else {
            mvwaddstr(write_win, row, 2, var.first.c_str());
        }

        if (in_state == IState::VALUE && writeSel != writeVars.end() && *writeSel == var) {
            mvwaddstr(write_win, row, 20, in_buf.c_str());
            wattron(write_win, A_STANDOUT);
            waddstr(write_win, " ");
            wattroff(write_win, A_STANDOUT);
        } else {
            if (var.second.type() == Variable::Type::FLOAT) {
                mvwprintw(write_win, row, 20, "%-7f", var.second.getFloat());
            } else {
                mvwprintw(write_win, row, 20, "%-7i", var.second.getInt());
            }
        }
        mvwprintw(write_win, row, 30, "%-3d", var.second.startReg());
        mvwprintw(write_win, row, 34, "%-1d", var.second.size());
        row++;
    }
    wnoutrefresh(write_win);

    doupdate();
}

class SimpleCallbacks : public DeviceCallbacks {
    public:
        void onRead(const std::unordered_set<BoundVariable>& vars) override {
            std::lock_guard<std::mutex> lck(varLock);
            for (auto var : vars) {
                readVars.at(var.name()) = var;
            }
        }

        void onWrite(const std::unordered_set<BoundVariable>& UNUSED(vars)) override {
            // don't need to do anything with write ACKs
        }

        bool getSoftKill() override {
            std::lock_guard<std::mutex> lck(varLock);
            return softKill;
        }

        void postHardKill(bool canHardKill, bool arg_hardKill) override {
            std::lock_guard<std::mutex> lck(varLock);
            hardKillValid = canHardKill;
            hardKill = arg_hardKill;
        }

        void onDisconnect() override {
            std::lock_guard<std::mutex> lck(varLock);
            devInfo = nullptr;
            readVars.clear();
            writeVars.clear();
            writeSel = writeVars.begin();
            curses_refresh_windows();
        }
};

static std::shared_ptr<DeviceCallbacks> onConnect(Manager* mgr, std::shared_ptr<DeviceInfo> info) {
    std::lock_guard<std::mutex> lck(varLock);
    devInfo = info;

    std::unordered_set<BoundVariable> writeSet;

    for (auto var : info->writeVariables()) {
        if (var.type() == Variable::Type::FLOAT) {
            BoundVariable bVar = var.bind(0.0f);
            writeVars.insert(std::make_pair(var.name(), bVar));
            writeSet.insert(bVar);
        } else {
            BoundVariable bVar = var.bind(0);
            writeVars.insert(std::make_pair(var.name(), bVar));
            writeSet.insert(bVar);
        }
    }

    writeSel = writeVars.begin();

    mgr->submitWrite(writeSet);

    for (auto var : info->readVariables()) {
        if (var.type() == Variable::Type::FLOAT) {
            readVars.insert(std::make_pair(var.name(), var.bind(0.0f)));
        } else {
            readVars.insert(std::make_pair(var.name(), var.bind(0)));
        }
    }

    // submit an initial read to grab all the read variables as well
    mgr->submitRead(info->readVariables());

    curses_refresh_windows();

    return std::make_shared<SimpleCallbacks>();
}

static void onPortFailed(Manager* UNUSED(mgr), std::string message) {
    std::lock_guard<std::mutex> lck(varLock);
    got_error = true;
    error_message = message;
    shouldEnd = true;
}

static void handle_value() {
    try {
        if (writeSel != writeVars.end()) {
            if (writeSel->second.type() == Variable::Type::FLOAT) {
                float val = std::stof(in_buf);
                BoundVariable newVal = writeSel->second.bind(val);
                writeSel->second = newVal;
                manager->submitWrite({newVal});
            } else {
                int val = std::stoi(in_buf);
                BoundVariable newVal = writeSel->second.bind(val);
                writeSel->second = newVal;
                manager->submitWrite({newVal});
            }
        }
    } catch (std::exception& e) {
        // ignore
    }
    in_buf.clear();
}


static int simpled_run(std::map<std::string, std::string> args) {
    std::string ioPath = args["port"];

    writeSel = writeVars.begin();

    try {
        manager = new Manager(ioPath, onConnect, onPortFailed);
    } catch (std::exception& e) {
        std::cout << "Failed to create manager: " << e.what() << std::endl;
        return -5;
    }

    manager->start();

    signal(SIGTERM, signal_exit);
    signal(SIGINT, signal_exit);

    curses_init();

    while (!shouldEnd) {
        // Process input
        int ch;
        while ((ch = getch()) != ERR) {
            if (in_state == IState::NORMAL) {
                switch (ch) {
                    case KEY_UP:
                        if (writeSel != writeVars.begin()) --writeSel;
                        break;
                    case KEY_DOWN:
                        if (writeSel != writeVars.end()) ++writeSel;
                        break;
                    case ' ':
                        softKill = !softKill;
                        break;
                    case KEY_ENTER:
                    case '\r':
                    case '\n':
                        if (writeSel != writeVars.end()) in_state = IState::VALUE;
                        break;
                    default:
                        break;
                }
            } else {
                // In a value input state
                if (ch == KEY_ENTER || ch == '\r' || ch == '\n') {
                    handle_value();
                    in_state = IState::NORMAL;
                } else if (ch == 27) { // Escape key
                    in_buf.clear();
                    in_state = IState::NORMAL;
                } else if (ch == KEY_BACKSPACE || ch == 127) {
                    if (!in_buf.empty()) in_buf.pop_back();
                } else {
                    in_buf.push_back(ch);
                }
            }
        }

        curses_update();

        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    curses_shutdown();

    manager->stop();

    delete manager;
    
    if (got_error) {
        std::cout << error_message << std::endl;
    }

    return 0;
}

BEGIN_COMMAND_DEF(simpled, "Connect interactively to a single device", simpled_run)
ARG_REQUIRED(port, "The device's port")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
