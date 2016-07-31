#include <unordered_map>
#include <tuple>
#include <random>
#include <memory>
#include <assert.h>
#include <string>

#include "passive.hpp"
#include "objects.hpp"
#include "simulator.hpp"

namespace cuauv {
namespace fishbowl {

class turbulence_manager : public object_manager {
public:
    turbulence_manager(simulator& sim);
    ~turbulence_manager();

    std::pair<bool, std::string> add(object_id id, bitreader& rd) override;
    std::pair<bool, std::string> set(object_id id, bitreader& rd) override;
    bool remove(object_id id) override;

private:
    simulator& sim;
    std::unordered_map<object_id, force_id> registry;
};

turbulence_manager::turbulence_manager(simulator& sim)
    : sim(sim)
{
}

turbulence_manager::~turbulence_manager()
{
}

std::pair<bool, std::string> turbulence_manager::add(object_id oid, bitreader& rd)
{
    assert(registry.count(oid) == 0);

    Eigen::Vector3d fa, fb, ta, tb;
    double fda, fdb, tda, tdb;

    fa[0] = rd.float64();
    fa[1] = rd.float64();
    fa[2] = rd.float64();

    fb[0] = rd.float64();
    fb[1] = rd.float64();
    fb[2] = rd.float64();

    fda = rd.float64();
    fdb = rd.float64();
    
    ta[0] = rd.float64();
    ta[1] = rd.float64();
    ta[2] = rd.float64();

    tb[0] = rd.float64();
    tb[1] = rd.float64();
    tb[2] = rd.float64();

    tda = rd.float64();
    tdb = rd.float64();

    rd.end();
    if (rd.failed()) {
        return {false, "Failed to parse turbulence config."};
    }

    std::uniform_real_distribution<double> fd(fda, fdb);
    std::uniform_real_distribution<double> td(tda, tdb);

    force* eng = new turbulence(fa, fb, fd, ta, tb, td);
    force_id id = sim.get_physics().add_force(eng);

    registry.insert({oid, id});
    return {true, ""};
}

std::pair<bool, std::string> turbulence_manager::set(object_id id, bitreader& rd)
{
    // Not yet implemented.
    return {false, "Turbulence config setting not yet implemented."};
}

bool turbulence_manager::remove(object_id id)
{
    if (registry.count(id) != 1) {
        return false;
    }

    sim.get_physics().remove_force(registry.at(id));
    registry.erase(id);
    return true;
}

object_manager_map init_object_manager_map(simulator& sim)
{
    object_manager_map om;

    om["turbulence"] = std::unique_ptr<object_manager>(new turbulence_manager(sim));

    return om;
}

} // namespace fishbowl
} // namespace cuauv
