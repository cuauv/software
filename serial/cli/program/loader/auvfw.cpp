#include "auvfw.h"

#include <sstream>

#include "proto/cpp/AUVFirmware.pb.h"

namespace cuauv {
namespace serial {
namespace cli {

AUVFirmware loadAUVfw(std::istream& in) {
    std::stringstream buffer;
    buffer << in.rdbuf();
    std::string in_string = buffer.str();
    
    proto::AUVFirmware pfw;
    if (!pfw.ParseFromString(in_string)) {
        throw std::invalid_argument("Input was not a valid AUV firmware file");
    }

    MemoryImage mem;
    for (auto chunk : pfw.chunks()) {
        auto data_str = chunk.data();
        std::vector<MemoryImage::byte_t> data(data_str.begin(), data_str.end());
        mem.addChunk(chunk.base(), data);
    }

    return AUVFirmware(pfw.devicename(),
                       pfw.sig1(),
                       pfw.sig2(),
                       pfw.sig3(),
                       pfw.pagesize(),
                       mem);
}

void dumpAUVfw(std::ostream& out, const AUVFirmware& fw) {
    proto::AUVFirmware pfw;
    pfw.set_devicename(fw.deviceName());
    pfw.set_sig1(fw.sig1());
    pfw.set_sig2(fw.sig2());
    pfw.set_sig3(fw.sig3());
    pfw.set_pagesize(fw.pageSize());

    for (auto chunk : fw.mem().getChunks()) {
        auto pchunk = pfw.add_chunks();
        pchunk->set_base(chunk.first);
        std::string data_str(chunk.second.begin(), chunk.second.end());
        pchunk->set_data(data_str);
    }

    out << pfw.SerializeAsString();
}

}}} // end namespace cuauv::serial::cli
