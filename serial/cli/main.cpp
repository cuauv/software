#include <iostream>
#include <functional>
#include <string>
#include <vector>
#include <map>

#include "Command.h"

using namespace cuauv::serial::cli;

int main(int argc, char* argv[]) {
    return CommandDispatcher::run(argc, argv);
}
