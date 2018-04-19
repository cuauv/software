#ifndef CUAUV_SIM_BITS_H
#define CUAUV_SIM_BITS_H

#include <stdint.h>

namespace cuauv {
namespace fishbowl {

uint32_t uint32_of_bytes(uint8_t* from);
uint64_t uint64_of_bytes(uint8_t* from);
int32_t int32_of_bytes(uint8_t* from);
int64_t int64_of_bytes(uint8_t* from);
double float32_of_bytes(uint8_t* from);
double float64_of_bytes(uint8_t* from);

void bytes_of_uint32(uint8_t* to, uint32_t from);
void bytes_of_uint64(uint8_t* to, uint64_t from);
void bytes_of_int32(uint8_t* to, int32_t from);
void bytes_of_int64(uint8_t* to, int64_t from);
void bytes_of_float32(uint8_t* to, float from);
void bytes_of_float64(uint8_t* to, double from);

} // namespace cuauv
} // namespace fishbowl

#endif // CUAUV_SIM_BITS_H
