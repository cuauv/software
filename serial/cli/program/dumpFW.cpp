#include <iostream>
#include <fstream>

#include "loader/MemoryImage.h"
#include "loader/AUVFirmware.h"
#include "loader/hex.h"
#include "loader/auvfw.h"

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

static int dumpFW_run(std::map<std::string, std::string> args) {
    try {
        std::ifstream fwin(args["fw"]);
        AUVFirmware fw = loadAUVfw(fwin);

        std::cout << "Device name: " << fw.deviceName() << std::endl;
        std::cout << "Signature bytes: " << (int) fw.sig1();
        std::cout << " " << (int) fw.sig2() << " " << (int) fw.sig3() << std::endl; 
        std::cout << "Page size: " << fw.pageSize() << std::endl;

        std::ofstream hexOut(args["out"]);
        dumpHex(hexOut, fw.mem());
        std::cout << "Memory image written to " << args["out"] << std::endl;
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(dumpFW, "Dump the contents of a firmware file", dumpFW_run)
ARG_REQUIRED(fw, "The firmware to dump")
ARG_REQUIRED(out, "The file to dump the hex-formatted memory image to")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
