#include <unordered_map>
#include <functional>
#include <stdexcept>

#include "entity.hpp"
#include "world.hpp"

namespace cuauv {
namespace fishbowl {

world::world()
    : next_entity_id(0)
    , next_entity_observer_id(0)
{ }

world::~world()
{
    for (auto it = entities.begin(); it != entities.end();) {
        notify_entity_observers(entity_event::REMOVED, it->first);
        entities.erase(it++);
    }
}

world::world(const world& w)
    : next_entity_id(w.next_entity_id)
    , next_entity_observer_id(0)
{
}

world::world(world&& other)
    : world()
{
    swap(*this, other);
}

world& world::operator=(world other)
{
    swap(*this, other);
    return *this;
}

void swap(world& first, world& second)
{
    using std::swap; // for ADL
    swap(first.next_entity_id, second.next_entity_id);
    swap(first.next_entity_observer_id, second.next_entity_observer_id);
    swap(first.entities, second.entities);
    swap(first.entity_observers, second.entity_observers);
}

entity_id world::add_entity(entity entity)
{
    entities.insert(std::make_pair(next_entity_id, entity));

    notify_entity_observers(entity_event::ADDED, next_entity_id);

    return next_entity_id++;
}

bool world::has_entity(entity_id id)
{
    return entities.count(id) == 1;
}

entity& world::get_entity(entity_id id)
{
    if (!has_entity(id)) {
        throw std::invalid_argument("No entity with given ID.");
    }
    
    return entities.at(id);
}

void world::remove_entity(entity_id id)
{
    if (!has_entity(id)) {
        throw std::invalid_argument("No entity with given ID.");
    }

    entities.erase(id);

    notify_entity_observers(entity_event::REMOVED, id);
}

entity_observer_id world::add_entity_observer(entity_observer obs)
{
    entity_observers[next_entity_observer_id] = obs;
    return next_entity_observer_id++;
}

void world::remove_entity_observer(entity_observer_id id)
{
    assert(entity_observers.count(id) == 1);
    entity_observers.erase(id);
}

void world::notify_entity_observers(entity_event event, entity_observer_id d)
{
    for (const auto& x : entity_observers) {
        x.second(event, d);
    }
}

} // namespace fishbowl
} // namespace cuauv
