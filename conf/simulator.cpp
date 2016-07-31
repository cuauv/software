#include <random>
#include <iostream>
#include <fstream>
#include <string>

#include <lib/json.h>

#include "parse.hpp"
#include "simulator.hpp"

namespace cuauv {
namespace conf {

#define PARSE_UNI_REAL_DIST(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected uniform real distribution " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 2 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble()) { \
        throw std::runtime_error("Expected vector of two doubles for " #name "."); \
    } \
    std::uniform_real_distribution<double> name(root[#name][0].asDouble(), root[#name][1].asDouble());

simulator load_simulator(const std::string& filename)
{
    char* dir = getenv("CUAUV_SOFTWARE");
    if (dir == nullptr) {
        throw std::runtime_error("cuauv::conf::load_simulator: CUAUV_SOFTWARE must be set to the root of the software repository, with a trailing slash.");
    }

    std::ifstream ifs(std::string(dir) + filename);
    if (!ifs) {
        throw std::runtime_error("cuauv::conf::load_simulator: Failed to open vehicle file.");
    }

    Json::Value root;
    Json::Reader reader;
    if (!reader.parse(ifs, root)) {
        throw std::runtime_error("cuauv::conf::load_simulator: Failed to parse vehicle file.");
    }

    simulator s {
    };

    return s;
}

} // namespace conf
} // namespace cuauv
