#include <stdint.h>
#include <iostream>
#include <string>

#include "bits.hpp"
#include "bitreader.hpp"

#define WRAP(type, fun, len) \
    if (_failed) return 0; \
    at += len; \
    if (at > length) { \
        _failed = true; \
        return 0; \
    } \
    type x = fun(from); \
    from += len; \
    return x;

namespace cuauv {
namespace fishbowl {

bitreader::bitreader(uint8_t* from, uint32_t length, uint32_t max_string_length)
    : from(from)
    , length(length)
    , max_string_length(max_string_length)
{
}

float bitreader::float32()
{
    WRAP(float, float32_of_bytes, 4);
}

double bitreader::float64()
{
    WRAP(double, float64_of_bytes, 8);
}


int32_t bitreader::int32()
{
    WRAP(int32_t, int32_of_bytes, 4);
}

int64_t bitreader::int64()
{
    WRAP(int64_t, int64_of_bytes, 8);
}

uint8_t bitreader::uint8()
{
    if (_failed) return 0;
    at++;
    if (at > length) {
        _failed = true;
        return 0;
    }
    uint8_t x = from[0];
    from++;
    return x;
}

uint32_t bitreader::uint32()
{
    WRAP(uint32_t, uint32_of_bytes, 4);
}

uint64_t bitreader::uint64()
{
    WRAP(uint64_t, uint64_of_bytes, 8);
}

bool bitreader::bool_()
{
    uint8_t x = uint8();
    if (_failed) return false;
    return x != 0;
}

std::string bitreader::string(bool u8len)
{
    if (_failed) return "";
    uint32_t len;
    if (u8len) {
        len = uint8();
    } else {
        len = uint32();
    }
    if (_failed || len == 0) {
        _failed = true;
        return "";
    }
    at += len;
    if (at > length) {
        _failed = true;
        return "";
    }
    std::string str(reinterpret_cast<char *>(from), len);
    from += len;
    return str;
}

void bitreader::end()
{
    if (at != length) {
        _failed = true;
    }
}

bool bitreader::failed()
{
    return _failed;
}

} // namespace fishbowl
} // namespace cuauv

#undef WRAP
