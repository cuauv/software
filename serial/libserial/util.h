#pragma once

#include <functional>

namespace cuauv
{
namespace serial
{

// Shamelessly stolen from boost::hash_combine (easier than pulling
// all of boost in as a dependency)
template <class T>
inline void hash_combine(std::size_t & seed, const T & v)
{
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

} // end namespace serial
} // end namespace cuauv
