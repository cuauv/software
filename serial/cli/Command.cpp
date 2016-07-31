#include "Command.h"

#include <iostream>

namespace cuauv {
namespace serial {
namespace cli {

int CommandDispatcher::run(int argc, char **argv) {
    return getInstance()->_run(argc, argv);
}

int CommandDispatcher::_run(int argc, char **argv) {
    std::list<std::string> args;
    std::string progName = argv[0];

    for (int i = 1; i < argc; i++) {
        args.push_back(argv[i]);
    }

    if (args.size() < 1) {
        std::cout << "ERROR: No command specified" << std::endl;
        printFullUsage(progName);
        return 1;
    }

    std::string cmd = args.front();
    args.pop_front();

    auto it = m_info.find(cmd);
    if (it == m_info.end()) {
        std::cout << "ERROR: Unknown command " << cmd << std::endl;
        printFullUsage(progName);
    }

    auto cmdInfo = it->second;
    std::map<std::string, std::string> argVals;
    for (auto arg : cmdInfo.args) {
        if (!args.empty()) {
            argVals.insert(std::make_pair(arg.name, args.front()));
            args.pop_front();
        } else {
            if (arg.required) {
                std::cout << "ERROR: Missing argument " << arg.name << std::endl;
                printCommandUsage(progName, cmdInfo);
                return 1;
            }
        }
    }

    return cmdInfo.func(argVals);
}

CommandDispatcher::Register::Register(CommandInfo info) {
    // Drop the last argument, it's just a dummy to make the macros work
    info.args.pop_back();

    // Register the command in the main array
    getInstance()->m_info.insert(std::make_pair(info.name, info));
}

CommandDispatcher* CommandDispatcher::getInstance() {
    // There's a small amount of non-obvious linker trickery going on here
    // Global constructors are executed in an arbitrary order, so there's no guarantee
    // that a global static instance of the CommandDispatcher object (or some other
    // data structure) would have its constructor run before the Register objects. However,
    // there are guarantees about construction order of static local variables. inst will
    // have its constructor run on the first call to this method. Since this call occurs
    // as the first step of registration, we are guaranteed that the CommandDispatcher constructor
    // has run before attempting to insert the command into it's map of commands
    static CommandDispatcher inst;
    return &inst;
}

void CommandDispatcher::printFullUsage(const std::string& progName) const {
    std::cout << "Usage: " << progName << " <command> [args]" << std::endl;
    std::cout << "Available commands: " << std::endl;
    for (auto cmd : m_info) {
        printShortCommandUsage(cmd.second);
    }
}

void CommandDispatcher::printShortCommandUsage(const CommandInfo& cmd) const {
    std::cout << "  " << cmd.name << ": " << cmd.description << std::endl;
}

void CommandDispatcher::printCommandUsage(const std::string& progName, const CommandInfo& cmd) const {
    printShortCommandUsage(cmd);
    std::cout << "  Usage: " << progName << " " << cmd.name;
    for (auto arg : cmd.args) {
        if (arg.required) {
            std::cout << " <" << arg.name << ">";
        } else {
            std::cout << " [" << arg.name << "]";
        }
    }
    std::cout << std::endl;
    for (auto arg : cmd.args) {
        std::cout << "    " << arg.name << ": " << arg.description << std::endl;
    }
}

}}} // end namespace cuauv::serial::cli
