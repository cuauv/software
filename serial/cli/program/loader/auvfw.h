#pragma once

#include <iostream>

#include "AUVFirmware.h"

namespace cuauv {
namespace serial {
namespace cli {

AUVFirmware loadAUVfw(std::istream& in);
void dumpAUVfw(std::ostream& out, const AUVFirmware& mem);

}}} // end namespace cuauv::serial::cli
