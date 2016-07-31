#ifndef CUAUV_SIM_THRUSTERS_H
#define CUAUV_SIM_THRUSTERS_H

#include <functional>

#include <Eigen/Geometry>

#include <libshm/c/shm.h>
#include <conf/vehicle.hpp>

#include "engine.hpp"
#include "pid.hpp"
#include "screw.hpp"

namespace cuauv {
namespace fishbowl {

class ciw_thrusters : public engine {
public:
    ciw_thrusters(const Eigen::Quaterniond& btom_rq);
    screw on();
    void step(double delta);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    const Eigen::Quaterniond& btom_rq;
    control_internal_wrench shm_wrench;
};

typedef std::function<screw()> passive_forces_func;

class des_thrusters : public engine {
public:
    des_thrusters(const Eigen::Quaterniond& q, const Eigen::Vector3d& v,
            const Eigen::Vector3d& x, const Eigen::Quaterniond& btom_rq,
            const cuauv::conf::vector6d& axes, const cuauv::conf::vector6d& mins,
            const cuauv::conf::vector6d& maxs, passive_forces_func pff, const Eigen::Matrix3d& I, double m);
    screw on() override;
    void step(double delta) override;

    // All these values are in the model frame.

    epid hp {0, 0, 0, 0}; //<! Heading Euler angle PID controller.
    epid pp {0, 0, 0, 0}; //<! Pitch Euler angle PID controller.
    epid rp {0, 0, 0, 0}; //<! Roll Euler angle PID controller.
    
    pid xp {0, 0, 0, 0}; //<! X-velocity PID controller.
    pid yp {0, 0, 0, 0}; //<! Y-velocity PID controller.
    pid dp {0, 0, 0, 0}; //<! Depth PID controller.

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    const Eigen::Quaterniond& q;
    const Eigen::Vector3d& v;
    const Eigen::Vector3d& x;

    Eigen::Matrix3d btom_rm;
    Eigen::Quaterniond mtob_rq;
    Eigen::Matrix3d mtob_rm;
    cuauv::conf::vector6d axes;
    double axesm; //<! The squared magnitude of [axes].

    // Force and torque limits, [a, b].
    Eigen::Vector3d fa;
    Eigen::Vector3d fb;
    Eigen::Vector3d ta;
    Eigen::Vector3d tb;

    passive_forces_func pff;

    Eigen::Matrix3d I;
    double m;

    settings_control shm_settings;

    desires shm_desires;
    settings_heading shm_hp;
    settings_pitch shm_pp;
    settings_roll shm_rp;
    settings_velx shm_xp;
    settings_vely shm_yp;
    settings_depth shm_dp;

    Eigen::Vector3d f {0, 0, 0};
    Eigen::Vector3d t {0, 0, 0};
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_THRUSTERS_H
