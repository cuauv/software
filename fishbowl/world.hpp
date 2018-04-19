#ifndef CUAUV_SIM_WORLD_H
#define CUAUV_SIM_WORLD_H

#include <unordered_map>
#include <functional>
#include <stdint.h>
#include <stdexcept>

#include "entity.hpp"

namespace cuauv {
namespace fishbowl {

/*
 * The world frame has x, y, z as north, east, down.
 *
 * The model frame has (1, 0, 0) as 1m from the center of mass of the vehicle,
 * (0, 1, 0) 1m to the right of the center of mass, and (0, 0, 1) as 1m above.
 *
 * The body frame has its axes as the principal axes of rotation of the vehicle,
 * determined from the eigenvectors of the inertia tensor.
 *
 * All units are metric unless otherwise noted.
 */

enum class entity_event {
    ADDED,
    REMOVED,
};

typedef uint32_t entity_id;
typedef uint32_t entity_observer_id;

typedef std::function<void(entity_event, entity_id)> entity_observer;

/**
 * Represents a world containing entities.
 *
 * Copying a world does not copy its entity observers. Moving a world moves its
 * entity observers.
 */
class world {
public:
    world();
    ~world();

    world(const world&);
    world(world&&);
    world& operator=(const world);
    friend void swap(world& first, world& second);

    entity_id add_entity(entity entity);
    bool has_entity(entity_id id);
    entity& get_entity(entity_id id);
    void remove_entity(entity_id id);

    entity_observer_id add_entity_observer(entity_observer obs);
    void remove_entity_observer(entity_id id);

    template<typename F>
    void for_each_entity(F f);

private:
    void notify_entity_observers(entity_event event, entity_id id);

    std::unordered_map<entity_id, entity> entities;
    entity_id next_entity_id;

    std::unordered_map<entity_observer_id, entity_observer> entity_observers;
    entity_observer_id next_entity_observer_id;
};

template<typename F>
void world::for_each_entity(F f)
{
    for (auto& x : entities) {
        f(x.first, x.second);
    }
}

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_WORLD_H
