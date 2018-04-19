#include <iostream>
#include <fstream>

#include "loader/MemoryImage.h"
#include "loader/hex.h"

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

static int mergeHex_run(std::map<std::string, std::string> args) {
    try {
        std::ifstream hex1(args["in1"]);
        std::ifstream hex2(args["in2"]);
        MemoryImage image = loadHex(hex1);
        image.merge(loadHex(hex2));

        std::ofstream hexOut(args["out"]);
        dumpHex(hexOut, image);
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(mergeHex, "Merge two hex files into one", mergeHex_run)
ARG_REQUIRED(in1, "The first file to merge")
ARG_REQUIRED(in2, "The second file to merge")
ARG_REQUIRED(out, "The file to output the merged memory image to")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
