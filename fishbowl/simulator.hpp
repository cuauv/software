#ifndef CUAUV_SIM_SIMULATOR_H
#define CUAUV_SIM_SIMULATOR_H

#include <atomic>
#include <vector>
#include <utility>
#include <stdexcept>
#include <mutex>
#include <condition_variable>

#include <conf/vehicle.hpp>

#include "spdlog/spdlog.h"

#include "world.hpp"
#include "physics.hpp"

namespace cuauv {
namespace fishbowl {

typedef uint32_t hook_id;

struct run_config {
    cuauv::conf::vehicle vehicle;
    double frequency;
    double speed;
    bool non_real_time;
    bool use_des_thrusters;
    bool verbose;
};

class simulator {
public:
    simulator(world& w, physics& p);
    int run(const run_config& config);
    void stop();

    world& get_world();
    physics& get_physics();

    hook_id add_hook(std::function<void()> hook);
    void remove_hook(hook_id id);

    // Require that the user holds the simulator lock.
    void pause();
    void unpause();
    bool paused();

    /**
     * Returns a std::unique_lock that can be used to lock simulator, world,
     * and physics for writing.
     * The returned lock is unlocked, but whether or not the underlying mutex
     * is locked is undefined.
     * Called by and affects simulator::run as well.
     */
    std::unique_lock<std::mutex> get_lock();

private:
    world& w;
    physics& p;

    std::shared_ptr<spdlog::logger> log;

    std::vector<std::pair<hook_id, std::function<void()>>> hooks;
    hook_id next_hook_id = 0;

    std::mutex mut;
    std::atomic_bool stopped { false };

    // Mutex for pausing.
    std::condition_variable cv;
    bool _paused = false;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_SIMULATOR_H
