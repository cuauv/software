#ifndef CUAUV_SIM_BITWRITER_H
#define CUAUV_SIM_BITWRITER_H

#include <stdint.h>
#include <string>

namespace cuauv {
namespace fishbowl {

class bitwriter {
public:
    bitwriter();
    ~bitwriter();

    void write(float x);
    void write(double x);
    void write(int32_t x);
    void write(int64_t x);
    void write(uint8_t x);
    void write(uint32_t x);
    void write(uint64_t x);
    void write(std::string x, bool u8len=false);
    void write(bool x);

    uint8_t* data();
    uint32_t size();

private:
    void ensure(uint32_t size);

    uint8_t* to;
    uint32_t length = 0;
    uint32_t at = 0;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_BITWRITER_H
