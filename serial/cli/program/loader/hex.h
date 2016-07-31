#pragma once

#include <iostream>

#include "MemoryImage.h"

namespace cuauv {
namespace serial {
namespace cli {

MemoryImage loadHex(std::istream& in);
void dumpHex(std::ostream& out, const MemoryImage& mem);

}}} // end namespace cuauv::serial::cli
