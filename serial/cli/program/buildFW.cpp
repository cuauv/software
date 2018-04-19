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

static int parse_num(std::string str) {
    if (str[0] == '0' && str[1] == 'x') {
        // parse as hex
        return std::stoul(str, nullptr, 16);
    } else {
        // parse as base 10
        return std::stoul(str);
    }
}

static int buildFW_run(std::map<std::string, std::string> args) {
    try {
        std::string deviceName = args["deviceName"];
        uint8_t sig1, sig2, sig3;
        uint16_t pageSize;
        try {
            sig1 = parse_num(args["sig1"]);
            sig2 = parse_num(args["sig2"]);
            sig3 = parse_num(args["sig3"]);
            pageSize = parse_num(args["pageSize"]);
        } catch (std::exception& e) {
            throw std::invalid_argument("Error: Non-integer argument");
        } 
        std::ifstream hex(args["hex"]);
        MemoryImage image = loadHex(hex);

        AUVFirmware fw(deviceName, sig1, sig2, sig3, pageSize, image);

        std::ofstream fwOut(args["out"]);
        dumpAUVfw(fwOut, fw);
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(buildFW, "Create an AUV firmware file", buildFW_run)
ARG_REQUIRED(deviceName, "The target device's name")
ARG_REQUIRED(sig1, "The target device's SIG1 byte")
ARG_REQUIRED(sig2, "The target device's SIG2 byte")
ARG_REQUIRED(sig3, "The target device's SIG3 byte")
ARG_REQUIRED(pageSize, "The target device's page size in words")
ARG_REQUIRED(hex, "The firmware hex file")
ARG_REQUIRED(out, "The output file")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
