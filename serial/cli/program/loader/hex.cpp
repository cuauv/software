#include "hex.h"

namespace cuauv {
namespace serial {
namespace cli {

static MemoryImage::byte_t parseHexDigit(char c) {
    if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else if (c >= '0' && c <= '9') {
        return c - '0';
    } else {
        throw std::invalid_argument("Invalid hex character " + std::to_string(c));
    }
}

static MemoryImage::byte_t parseHex(char charH, char charL) {
    return (parseHexDigit(charH) << 4) | parseHexDigit(charL);
}

static char toHexDigit(uint8_t d) {
    if (d < 10) {
        return '0' + d;
    } else {
        return 'A' + (d - 10);
    }
}

static void writeHex(std::ostream& out, uint8_t val) {
    out << toHexDigit((val >> 4) & 0x0F);
    out << toHexDigit(val & 0x0F);
}


MemoryImage loadHex(std::istream& in) {
    MemoryImage mem;

    bool gotEndRecord = false;
    while (in) {        
        std::string line;
        getline(in, line);
        // Skip if record is too short or not odd
        if (line.size() < 11) continue;
        // Skip if record start marker is missing
        if (line[0] != ':') continue;
        // avr-objcopy outputs hex files in DOS format
        // make sure the \r doesn't get pulled in
        if (line[line.size()-1] == '\r') line.pop_back();

        // Convert the record into bytes
        std::vector<MemoryImage::byte_t> record;
        int num_bytes = (line.size() - 1) / 2;
        record.reserve(num_bytes);
        for (int i = 0; i < num_bytes; i++) {
            MemoryImage::byte_t b = parseHex(line[2*i+1], line[2*i+2]);
            record.push_back(b);
        }

        // Check the checksum
        MemoryImage::byte_t sum = 0;
        for (auto b : record) {
            sum += b;
        }
        if (sum != 0) throw std::invalid_argument("Hex checksum error");

        uint8_t length = record[0];
        uint16_t address = ((uint16_t) record[1]) << 8 | record[2];
        uint8_t type = record[3];

        if (length != record.size() - 5) throw std::invalid_argument("Invalid record length");
        if (type == 0) {
            // Data type
            std::vector<MemoryImage::byte_t> data(record.begin()+4, record.begin()+4+length);
            mem.addChunk(address, data);
        } else if (type == 1) {
            gotEndRecord = true;
            break;
        }
        // Otherwise unsupported record type, ignore it
    }
    if (!gotEndRecord) {
        throw std::invalid_argument("Hex file did not have an EOF record");
    }

    return mem;
}

void dumpHex(std::ostream& out, const MemoryImage& mem) {
    for (auto chunk : mem.getChunks()) {
        size_t n = 0;
        auto base = chunk.first;
        while (n < chunk.second.size()) {
            MemoryImage::byte_t sum = 0;
            auto count = (MemoryImage::byte_t) std::min((size_t) 16, chunk.second.size() - n);

            out << ':';
            writeHex(out, count);
            sum += count;
            writeHex(out, base >> 8);
            sum += (base >> 8);
            writeHex(out, base & 0xFF);
            sum += base & 0xFF;
            writeHex(out, 0);

            for (size_t i = 0; i < count; i++) {
                auto b = chunk.second[n + i];
                writeHex(out, b);
                sum += b;
            }

            writeHex(out, 0 - sum);
            base += count;
            n += count;
            out << std::endl;
        }
    }
    // Write EOF record
    out << ":00000001FF" << std::endl;
}

}}} // end namespace cuauv::serial::cli
