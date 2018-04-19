#ifndef CUAUV_SIM_FORCE_H
#define CUAUV_SIM_FORCE_H

#include "entity.hpp"
#include "world.hpp"
#include "screw.hpp"

namespace cuauv {
namespace fishbowl {

class force {
public:
    /**
     * Returns the forces and torques on a given entity at the current timestep
     * in the world frame.
     */
    virtual screw on(entity_id id) = 0;

    /**
     * Advances to the next timestep.
     *
     * Should be called after relevant changes in world state. May cause cached
     * forces and torques on entities to be recalculated.
     */
    virtual void step(double delta) = 0;
    virtual ~force() {};
};

}
}

#endif // CUAUV_SIM_FORCE_H
