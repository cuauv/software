#include <endian.h>
#include <stdint.h>
#include <string.h>

namespace cuauv {
namespace fishbowl {

uint32_t uint32_of_bytes(uint8_t* from)
{
    uint64_t tmp;
    tmp = static_cast<uint32_t>(from[0]) << 24
        | static_cast<uint32_t>(from[1]) << 16
        | static_cast<uint32_t>(from[2]) << 8
        | static_cast<uint32_t>(from[3]);
    return tmp;
}

uint64_t uint64_of_bytes(uint8_t* from)
{
    uint64_t tmp;
    tmp = static_cast<uint64_t>(from[0]) << 56
        | static_cast<uint64_t>(from[1]) << 48
        | static_cast<uint64_t>(from[2]) << 40
        | static_cast<uint64_t>(from[3]) << 32
        | static_cast<uint64_t>(from[4]) << 24
        | static_cast<uint64_t>(from[5]) << 16
        | static_cast<uint64_t>(from[6]) << 8
        | static_cast<uint64_t>(from[7]);
    return tmp;
}

int32_t int32_of_bytes(uint8_t* from)
{
    int32_t res;
    uint32_t tmp = uint32_of_bytes(from);
    memcpy(&res, &tmp, 4);
    return res;
}

int64_t int64_of_bytes(uint8_t* from)
{
    int64_t res;
    uint64_t tmp = uint64_of_bytes(from);
    memcpy(&res, &tmp, 8);
    return res;
}

float float32_of_bytes(uint8_t* from)
{
    float res;
    uint32_t tmp = uint32_of_bytes(from);
    memcpy(&res, &tmp, 4);
    return res;
}

double float64_of_bytes(uint8_t* from)
{
    double res;
    uint64_t tmp = uint64_of_bytes(from);
    memcpy(&res, &tmp, 8);
    return res;
}

void bytes_of_uint32(uint8_t* to, uint32_t from)
{
    to[0] = (0xFF << 24 & from) >> 24;
    to[1] = (0xFF << 16 & from) >> 16;
    to[2] = (0xFF << 8 & from) >> 8;
    to[3] = 0xFF & from;
}

void bytes_of_uint64(uint8_t* to, uint64_t from)
{
    // 0xFFUL isn't guaranteed to be 64-bit, and otherwise we need a lot of
    // ugly static_cast<>s...
    uint64_t FF = 0xFF;
    to[0] = (FF << 56 & from) >> 56;
    to[1] = (FF << 48 & from) >> 48;
    to[2] = (FF << 40 & from) >> 40;
    to[3] = (FF << 32 & from) >> 32;
    to[4] = (FF << 24 & from) >> 24;
    to[5] = (FF << 16 & from) >> 16;
    to[6] = (FF << 8 & from) >> 8;
    to[7] = FF & from;
}

void bytes_of_int32(uint8_t* to, int32_t from)
{
    uint32_t tmp;
    memcpy(&tmp, &from, 4);
    bytes_of_uint32(to, tmp);
}

void bytes_of_int64(uint8_t* to, int64_t from)
{
    uint64_t tmp;
    memcpy(&tmp, &from, 8);
    bytes_of_uint64(to, tmp);
}

void bytes_of_float32(uint8_t* to, float from)
{
    uint32_t tmp;
    memcpy(&tmp, &from, 4);
    bytes_of_uint32(to, tmp);
}

// Populates uint8_t* [to] with the bytes from [from].
// [from] is interpreted as a uint64_t, and the bytes are assigned
// to [to] in big-endian ordering.
// When used with floats, assumes that float endianness is the same
// as for integers.
// NB: can't do *reinterpret_cast<uint32_t*>(&from), messes with compiler's
// strict aliasing optimizations...
void bytes_of_float64(uint8_t* to, double from)
{
    uint64_t tmp;
    memcpy(&tmp, &from, 8);
    bytes_of_uint64(to, tmp);
}

} // namespace fishbowl
} // namespace cuauv
