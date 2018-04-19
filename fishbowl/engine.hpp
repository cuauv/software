#ifndef CUAUV_SIM_ENGINE_H
#define CUAUV_SIM_ENGINE_H

#include "screw.hpp"

namespace cuauv {
namespace fishbowl {

class engine {
public:
    /**
     * Returns the forces and torques on at the current timestep in the
     * body frame.
     */
    virtual screw on() = 0;
    virtual void step(double delta) = 0;
    virtual ~engine() {};
};

}
}

#endif // CUAUV_SIM_ENGINE_H
