#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>

#include "bitwriter.hpp"
#include "bits.hpp"
#include "util.hpp"

#define WRAP(fun, len) \
    ensure(len); \
    fun(to + at, x); \
    at += len;

namespace cuauv {
namespace fishbowl {

// Undefined behavior if < 1.
constexpr double BITWRITER_INCREASE_FACTOR = 2;

bitwriter::bitwriter()
{
    // using malloc here so we can use realloc later
    to = static_cast<uint8_t*>(malloc(16));
    length = 16;
}

bitwriter::~bitwriter()
{
    free(to);
}

void bitwriter::write(float x)
{
    WRAP(bytes_of_float32, 4);
}

void bitwriter::write(double x)
{
    WRAP(bytes_of_float64, 8);
}

void bitwriter::write(int32_t x)
{
    WRAP(bytes_of_int32, 4);
}

void bitwriter::write(int64_t x)
{
    WRAP(bytes_of_int64, 8);
}

void bitwriter::write(uint8_t x)
{
    ensure(1);
    to[at] = x;
    at++;
}

void bitwriter::write(uint32_t x)
{
    WRAP(bytes_of_uint32, 4);
}

void bitwriter::write(uint64_t x)
{
    WRAP(bytes_of_uint64, 8);
}

void bitwriter::write(std::string x, bool u8len)
{
    if (u8len) {
        write(static_cast<uint8_t>(x.size()));
    } else {
        write(static_cast<uint32_t>(x.size()));
    }
    ensure(x.size());
    memcpy(to + at, x.data(), x.size());
    at += x.size();
}

void bitwriter::write(bool x)
{
    write(static_cast<uint8_t>(x ? 1 : 0));
}

uint8_t* bitwriter::data()
{
    return to;
}

uint32_t bitwriter::size()
{
    return at;
}

void bitwriter::ensure(uint32_t size)
{
    if (at + size > length) {
        uint32_t new_length = length * BITWRITER_INCREASE_FACTOR;
        if (at + size > new_length) {
            new_length = at + size;
        }

        uint8_t* res = static_cast<uint8_t*>(realloc(static_cast<void*>(to), new_length));
        if (res == nullptr) {
            // failed to realloc(), to not free()d
            MALLOC_FAILED(bitwriter::ensure);
        }
        // realloc()ed successfully, to freed
        to = res;
        length = new_length;
    }
}

} // namespace fishbowl
} // namespace cuauv

#undef WRAP
