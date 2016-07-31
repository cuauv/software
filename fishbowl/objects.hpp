#ifndef CUAUV_SIM_OBJECTS_H
#define CUAUV_SIM_OBJECTS_H

#include <unordered_map>
#include <memory>
#include <tuple>

#include "bitreader.hpp"
#include "simulator.hpp"

namespace cuauv {
namespace fishbowl {

typedef uint32_t object_id;

class object_manager {
public:
    virtual std::pair<bool, std::string> add(object_id id, bitreader& rd) = 0;
    virtual std::pair<bool, std::string> set(object_id id, bitreader& rd) = 0;
    virtual bool remove(object_id id) = 0;
    virtual ~object_manager() {};
};

typedef std::unordered_map<std::string, std::unique_ptr<object_manager>> object_manager_map;

object_manager_map init_object_manager_map(simulator& sim);

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_OBJECTS_H
