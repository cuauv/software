#ifndef CUAUV_SIM_PHYSICS_H
#define CUAUV_SIM_PHYSICS_H

#include <memory>
#include <unordered_map>
#include <utility>
#include <stdexcept>
#include <vector>

#include "engine.hpp"
#include "force.hpp"
#include "world.hpp"

namespace cuauv {
namespace fishbowl {

typedef uint32_t force_id;
typedef uint32_t engine_id;

class physics {
public:
    explicit physics(world& w);
    ~physics();

    // Disable copy construction/assignment, as we have unique_ptrs to forces,
    // engines, etc., for which copying might not make sense. Maybe revisit in 
    // the future.
    physics(const physics& w) = delete;
    physics& operator=(const physics& w) = delete;

    // NB: We assume ownership of the passed pointers.

    force_id add_force(force* force);
    bool has_force(force_id id);
    void remove_force(force_id id);

    engine_id add_engine(entity_id id, engine* engine);
    bool has_engine(engine_id id);
    void remove_engine(engine_id id);

    void step(double delta);

    void on_entity_event(entity_event event, entity_id id);

private:
    world& w;
    entity_observer_id w_obs_id;

    std::unordered_map<force_id, std::unique_ptr<force>> forces;
    force_id next_force_id;

    std::unordered_map<engine_id, std::pair<entity_id, std::unique_ptr<engine>>> engines;
    std::unordered_map<entity_id, std::vector<engine_id>> entity_engines;
    force_id next_engine_id;
};

} // namespace cuauv
} // namespace fishbowl

#endif // CUAUV_SIM_PHYSICS_H
