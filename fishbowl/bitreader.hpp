#ifndef CUAUV_SIM_BITREADER_H
#define CUAUV_SIM_BITREADER_H

#include <stdint.h>
#include <string>

namespace cuauv {
namespace fishbowl {

class bitreader {
public:
    bitreader(uint8_t* from, uint32_t length, uint32_t max_string_length);

    // Disable copy construction/assignment, as we have unique_ptrs to forces,
    bitreader(const bitreader& w) = delete;
    bitreader& operator=(const bitreader& w) = delete;

    float float32();
    double float64();
    int32_t int32();
    int64_t int64();
    uint8_t uint8();
    uint32_t uint32();
    uint64_t uint64();
    std::string string(bool u8len=false);
    bool bool_();
    void end();

    bool failed();

private:
    uint8_t* from;
    uint32_t length;
    uint32_t max_string_length;
    uint32_t at = 0;
    bool _failed = false;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_BITREADER_H

