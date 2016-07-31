#include <iostream>
#include <fstream>
#include <sstream>
#include <exception>
#include <chrono>
#include <vector>

#include <curses.h>
#include <signal.h>

#include <UnixFilePort.h>

#include "FakeDevice.h"

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

static bool shouldEnd = false;
static FakeDevice *dev = nullptr;
static std::shared_ptr<DeviceInfo> devInfo = nullptr;
static std::map<std::string, BoundVariable> readVars;
static std::map<std::string, BoundVariable>::iterator readSel;

static void signal_exit(int UNUSED(signum)) {
    shouldEnd = true;
}

static WINDOW *info_win, *read_win, *write_win;

enum class IState { NORMAL, VALUE };
static IState in_state = IState::NORMAL;
static std::string in_buf;

#define WINDOW_WIDTH 40

static void curses_init() {
    initscr();
    cbreak();
    noecho();
    nonl();
    intrflush(stdscr, FALSE);
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);
    curs_set(0);

    info_win = newwin(4, WINDOW_WIDTH, 0, 0);
    auto read_win_height = readVars.size() + 3;
    read_win = newwin(read_win_height, WINDOW_WIDTH, 4, 0);
    auto write_win_height = devInfo->writeVariables().size() + 3;
    write_win = newwin(write_win_height, WINDOW_WIDTH, 4 + read_win_height, 0);
}

static void curses_shutdown() {
    delwin(write_win);
    delwin(read_win);
    delwin(info_win);

    endwin();
}

static void curses_update() {
    box(info_win, 0, 0);
    mvwprintw(info_win, 1, 2, "Name: %s", devInfo->name().c_str());
    mvwprintw(info_win, 2, 2, "Type: %s", devInfo->type().c_str());

    if (dev->getHardKill()) {
        wattron(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 1, 28, "HARD");

    if (dev->getSoftKill()) {
        wattron(info_win, A_STANDOUT);
    } else {
        wattroff(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 2, 28, "SOFT");

    if (dev->getHardKillValid()) {
        wattron(info_win, A_STANDOUT);
    } else {
        wattroff(info_win, A_STANDOUT);
    }
    mvwaddstr(info_win, 1, 33, "VALID");

    if (dev->isConnected()) {
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
        if (readSel != readVars.end() && *readSel == var) {
            wattron(read_win, A_STANDOUT);
            mvwaddstr(read_win, row, 2, var.first.c_str());
            wattroff(read_win, A_STANDOUT);
        } else {
            mvwaddstr(read_win, row, 2, var.first.c_str());
        }

        if (in_state == IState::VALUE && readSel != readVars.end() && *readSel == var) {
            mvwaddstr(read_win, row, 20, in_buf.c_str());
            wattron(read_win, A_STANDOUT);
            waddstr(read_win, " ");
            wattroff(read_win, A_STANDOUT);
        } else {
            if (var.second.type() == Variable::Type::FLOAT) {
                mvwprintw(read_win, row, 20, "%-7f", var.second.getFloat());
            } else {
                mvwprintw(read_win, row, 20, "%-7i", var.second.getInt());
            }
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
    for (const auto& var : dev->getWriteVars()) {
        mvwaddstr(write_win, row, 2, var.first.c_str());

        if (var.second.type() == Variable::Type::FLOAT) {
            mvwprintw(write_win, row, 20, "%-7f", var.second.getFloat());
        } else {
            mvwprintw(write_win, row, 20, "%-7i", var.second.getInt());
        }
        mvwprintw(write_win, row, 30, "%-3d", var.second.startReg());
        mvwprintw(write_win, row, 34, "%-1d", var.second.size());
        row++;
    }
    wnoutrefresh(write_win);

    doupdate();
}

static void handle_value() {
    try {
        if (readSel != readVars.end()) {
            if (readSel->second.type() == Variable::Type::FLOAT) {
                float val = std::stof(in_buf);
                BoundVariable newVal = readSel->second.bind(val);
                readSel->second = newVal;
                dev->setReadVar(newVal);
            } else {
                int val = std::stoi(in_buf);
                BoundVariable newVal = readSel->second.bind(val);
                readSel->second = newVal;
                dev->setReadVar(newVal);
            }
        }
    } catch (std::exception& e) {
        // ignore
    }
    in_buf.clear();
}

static int fakedev_run(std::map<std::string, std::string> args) {
    std::string ioPath = args["port"];
    std::string cfgFile = args["config"];

    // Load device info
    try {
        std::ifstream protoFile(cfgFile);
        if (!protoFile.is_open()) {
            std::cout << "Failed to open config file" << std::endl;
            return -2;
        }

        std::stringstream buffer;
        buffer << protoFile.rdbuf();
        std::string info_string = buffer.str();
        protoFile.close();

        devInfo = std::make_shared<DeviceInfo>(0x00, 0x00, info_string);
    } catch (std::exception& e) {
        std::cout << "Failed to load config: " << e.what() << std::endl;
        return -3;
    }

    // Build the map of read variables
    for (auto var : devInfo->readVariables()) {
        if (var.type() == Variable::Type::FLOAT) {
            readVars.insert(std::make_pair(var.name(), var.bind(0.0f)));
        } else {
            readVars.insert(std::make_pair(var.name(), var.bind(0)));
        }
    }

    readSel = readVars.begin();

    try {
        dev = new FakeDevice(ioPath, devInfo);
    } catch (std::exception& e) {
        std::cout << "Failed to create device: " << e.what() << std::endl;
        return -4;
    }

    dev->start();

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
                        if (readSel != readVars.begin()) --readSel;
                        break;
                    case KEY_DOWN:
                        if (readSel != readVars.end()) ++readSel;
                        break;
                    case ' ':
                        dev->setHardKill(!dev->getHardKill());
                        break;
                    case 'v':
                        dev->setHardKillValid(!dev->getHardKillValid());
                        break;
                    case KEY_ENTER:
                    case '\r':
                    case '\n':
                        if (readSel != readVars.end()) in_state = IState::VALUE;
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

    dev->stop();

    delete dev;

    return 0;
}

BEGIN_COMMAND_DEF(fakedev, "Run a simulated device", fakedev_run)
ARG_REQUIRED(port, "The fake device's port")
ARG_REQUIRED(config, "The fake device's config")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
