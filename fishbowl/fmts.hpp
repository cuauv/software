#ifndef CUAUV_SIM_FMTS_H
#define CUAUV_SIM_FMTS_H

#include <memory>
#include <iostream>
#include <string>
#include <stdio.h>

// Originally from http://stackoverflow.com/a/26221725 .

namespace cuauv {
namespace fishbowl {

template<typename... Args>
std::string fmts(const std::string& format, Args... args)
{
    size_t size = 1 + snprintf(nullptr, 0, format.data(), args...);
    std::unique_ptr<char[]> buf(new char[size]);
    snprintf(buf.get(), size, format.data(), args...);
    return std::string(buf.get(), buf.get() + size);
}

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_FMTS_H
