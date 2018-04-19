#include <functional>
#include <iostream>
#include <math.h>
#include <time.h>
#include <unistd.h>
#include <stdexcept>

#include <Eigen/Core>

#include <conf/vehicle.hpp>
#include <libshm/c/shm.h>

#include "engine.hpp"
#include "entity.hpp"
#include "passive.hpp"
#include "physics.hpp"
#include "screw.hpp"
#include "thrusters.hpp"
#include "util.hpp"
#include "world.hpp"
#include "simulator.hpp"
#include "vision.hpp"
#include "geometry.hpp"

#define RAD_TO_DEG 57.29578

#define LOG(expr) \
    do { \
        if (log.get() != nullptr) { \
            log->expr; \
        } \
    } while (0);

namespace cuauv {
namespace fishbowl {

simulator::simulator(world& w, physics& p)
    : w(w)
    , p(p)
{
}

void simulator::stop()
{
    stopped = true;
}

static entity& set_default_environment(world& w, physics& p, const run_config& cfg)
{
    namespace conf = cuauv::conf;
    using namespace cuauv::fishbowl;

    const conf::vehicle& v = cfg.vehicle;

    gravity* fg = new gravity(w);
    p.add_force(fg);

    entity_id id = w.add_entity(entity(v.gravity_force / 9.81, 1.0, inertia_tensor(v.Ib), v.btom_rq));
    entity& e = w.get_entity(id);
    e.x = Eigen::Vector3d(0, 0, 0);
    e.q = v.btom_rq;

    Eigen::Matrix3d mtob_rm = v.btom_rq.conjugate().matrix();

    std::vector<drag*> drag_planes;
    for (const conf::drag_plane& dp : v.drag_planes) {
        drag* drag_plane = new drag(e.q, e.v, e.w, mtob_rm * dp.x, mtob_rm * dp.n, dp.c, dp.a);
        drag_planes.push_back(drag_plane);

        p.add_engine(id, drag_plane);
    }

    for (const conf::drag_plane& dp : v.uncompensated_drag_planes) {
        drag* drag_plane = new drag(e.q, e.v, e.w, mtob_rm * dp.x, mtob_rm * dp.n, dp.c, dp.a);
        p.add_engine(id, drag_plane);
    }

    buoyancy* fb = new buoyancy(e.q, v.buoyancy_force, mtob_rm * v.center_of_buoyancy);
    p.add_engine(id, fb);

    // ENABLE THRUSTERS!
    if (cfg.use_des_thrusters) {
        passive_forces_func pff = [&e, id, fg, drag_planes, fb, &v]() {
            screw s(Eigen::Vector3d(0, 0, 0), Eigen::Vector3d(0, 0, 0));

            s += fg->on(id);
            s = e.q.conjugate() * s;

            for (auto drag_plane : drag_planes) {
                s += drag_plane->on();
            }

            s += fb->on();

            return s;
        };
        des_thrusters* thrusters = new des_thrusters(e.q, e.v, e.x,
                v.btom_rq, v.cwhe_axes, v.thruster_minimums, v.thruster_maximums, pff, v.I, e.get_m());
        p.add_engine(id, thrusters);
    } else {
        p.add_engine(id, new ciw_thrusters(v.btom_rq));
    }

    return e;
}

const Eigen::Vector3d K(0, 0, 1);

int simulator::run(const run_config& cfg)
{
    entity& e = set_default_environment(w, p, cfg);

    kalman k;
    dvl dvl_g;
    shm_getg(dvl, dvl_g);

    double forward = 0;
    double sway = 0;

    // the quantum of simulation (in simulation time)
    const double steps = cfg.speed / cfg.frequency;
    // the timestep (in real time, microseconds)
    const long stepus = 1000000 / cfg.frequency;

    timespec tsa;
    timespec tsb;

    auto lk = get_lock();

    for (;;) {
        // to be removed in the future: break if v[0] is NaN
        // seems to occur under extreme conditions, not sure if our bug or controller bug
        if (e.v[0] != e.v[0]) {
            LOG(critical("NaN encountered. Terminating."));
            break;
        }

        lk.lock();

        if (_paused) {
            cv.wait(lk, [this] { return !(this->_paused); });
        }

        if (stopped) {
            break;
        }

        // SIMULATOR

        clock_gettime(CLOCK_MONOTONIC, &tsa);
        p.step(steps);
        clock_gettime(CLOCK_MONOTONIC, &tsb);

        const Eigen::Quaterniond q = e.get_model_q();
        const Eigen::Vector3d euler = quat_to_euler(q);

        // Component of the model quaternion about the z-axis (heading-only rotation).
        Eigen::Quaterniond qh = euler_to_quat(euler[2], 0, 0);

        // Velocity in heading modified world space.
        const Eigen::Vector3d vp = qh.conjugate() * e.v;
        // Output angular velocity and accelerations in the model frame.
        const Eigen::Vector3d wp = e.get_btom_rm() * e.w;
        const Eigen::Vector3d ap = e.get_model_q().conjugate() * e.a;

        k.accelx = ap[0];
        k.accely = ap[1];
        k.accelz = ap[2];
        k.depth = e.x[2];
        k.depth_rate = vp[2];
        k.east = e.x[1];
        forward += vp[0] * steps;
        k.forward = forward;
        k.heading = euler[2] * RAD_TO_DEG;
        k.heading_rate = wp[2] * RAD_TO_DEG;
        k.north = e.x[0];
        k.pitch = euler[1] * RAD_TO_DEG;
        k.pitch_rate = wp[1] * RAD_TO_DEG;
        k.q0 = q.w();
        k.q1 = q.x();
        k.q2 = q.y();
        k.q3 = q.z();
        k.roll = euler[0] * RAD_TO_DEG;
        k.roll_rate = wp[0] * RAD_TO_DEG;
        sway += vp[1] * steps;
        k.sway = sway;
        k.velx = vp[0];
        k.vely = vp[1];
        k.velz = vp[2];
        shm_setg(kalman, k);

        // Hackily set DVL altitude.
        dvl_g.savg_altitude = 4.0 - k.depth;
        shm_setg(dvl, dvl_g);

        // HOOKS

        for (auto& h : hooks) {
            h.second();
        }

        lk.unlock();

        // SLEEP

        // note: tv_nsec is only the nanosecond component of the elapsed time,
        // and must be added, x1e9, to tv_sec to get the "full" time.
        long elapsed = (tsb.tv_sec * 1000000 + tsb.tv_nsec / 1000) - (tsa.tv_sec * 1000000 + tsa.tv_nsec / 1000);

        if (cfg.non_real_time) {
            if (cfg.verbose) {
                // This can actually happen when we run too fast...
                if (elapsed == 0) {
                    std::cout << "Time elapsed: < 1 µs. Running at > 1000000 Hz." << std::endl;
                } else {
                    std::cout << "Time elapsed: " << elapsed << " µs. "
                              << "Running at " << 1000000 / elapsed << " Hz." << std::endl;
                }
            }
        } else {
            if (elapsed >= stepus) {
                std::cout << "Exceeded permitted step time! Time elapsed: " << elapsed << " µs, time permitted: " << stepus << "µs." << std::endl;
            } else {
                if (cfg.verbose) {
                    std::cout << "Sleeping " << stepus - elapsed << " µs." << std::endl;
                }

                usleep(stepus - elapsed);
            }
        }
    }

    return 0;
}

world& simulator::get_world()
{
    return w;
}

physics& simulator::get_physics()
{
    return p;
}

hook_id simulator::add_hook(std::function<void()> hook)
{
    hooks.push_back({ next_hook_id, hook });
    return next_hook_id++;
}

void simulator::remove_hook(hook_id id)
{
    for (auto it = hooks.begin(); it != hooks.end(); it++) {
        if ((*it).first == id) {
            hooks.erase(it);
            return;
        }
    }
    throw std::invalid_argument("No hook with given ID.");
}

std::unique_lock<std::mutex> simulator::get_lock()
{
    return std::unique_lock<std::mutex>(mut, std::defer_lock);
}

void simulator::pause()
{
    if (_paused) {
        throw std::runtime_error("Cannot pause when already paused.");
    }
    _paused = true;
}

void simulator::unpause()
{
    if (!_paused) {
        throw std::runtime_error("Cannot unpause when not paused.");
    }
    _paused = false;
    cv.notify_all();
}

bool simulator::paused()
{
    return _paused;
}

} // namespace fishbowl
} // namespace cuauv

#undef LOG
