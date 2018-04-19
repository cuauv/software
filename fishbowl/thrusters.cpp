#include <utility>
#include <math.h>
#include <functional>

#include <libshm/c/shm.h>
#include <conf/vehicle.hpp>

#include <iostream>

#include "thrusters.hpp"
#include "util.hpp"
#include "screw.hpp"
#include "geometry.hpp"

#define RAD_TO_DEG 57.29578
#define DEG_TO_RAD 0.017453293

#define SHM_GET_PID(from, tmp, to) \
    shm_getg(from, tmp); \
    to.p = tmp.kP; \
    to.i = tmp.kI; \
    to.d = tmp.kD; \
    to.r = tmp.rD;

#define SHM_PUT_PID(to, outv) \
    shm_set(to, out, outv);

namespace cuauv {
namespace fishbowl {

ciw_thrusters::ciw_thrusters(const Eigen::Quaterniond& btom_rq)
    : btom_rq(btom_rq)
{ }

screw ciw_thrusters::on()
{
    const Eigen::Vector3d f(shm_wrench.f_x, shm_wrench.f_y, shm_wrench.f_z);
    const Eigen::Vector3d t(shm_wrench.t_x, shm_wrench.t_y, shm_wrench.t_z);
    return screw(btom_rq.conjugate() * f, btom_rq.conjugate() * t);
}

void ciw_thrusters::step(double delta)
{
    shm_getg(control_internal_wrench, shm_wrench);
}

des_thrusters::des_thrusters(const Eigen::Quaterniond& q,
        const Eigen::Vector3d& v, const Eigen::Vector3d& x,
        const Eigen::Quaterniond& btom_rq, const cuauv::conf::vector6d& axes,
        const cuauv::conf::vector6d& mins, const cuauv::conf::vector6d& maxs,
        passive_forces_func pff, const Eigen::Matrix3d& I, double m)
    : q(q)
    , v(v)
    , x(x)
    , btom_rm(btom_rq.matrix())
    , mtob_rq(btom_rq.conjugate())
    // converting to a matrix has an upfront cost but saves ops in the long run
    // when applying to vectors
    , mtob_rm(btom_rq.conjugate().matrix())
    , axes(axes)
    , axesm(axes.dot(axes))
    , fa(mins.block<3, 1>(0, 0))
    , fb(maxs.block<3, 1>(0, 0))
    , ta(mins.block<3, 1>(3, 0))
    , tb(maxs.block<3, 1>(3, 0))
    , pff(pff)
    , I(I)
    , m(m)
{ }

screw des_thrusters::on() 
{
    return screw(f, t);
}

// clamps x to the range [a, b]
template<typename T>
void clamp(T& x, const T a, const T b)
{
    x = std::max(a, std::min(x, b));
}

// clamp clamps the values x_i to the ranges [a_i, b_i], i in [0, n)
void clamp(Eigen::Vector3d& x, const Eigen::Vector3d& a, const Eigen::Vector3d& b, int n)
{
    for (int i = 0; i < n; i++) {
        clamp(x[i], a[i], b[i]);
    }
}

const Eigen::Vector3d K(0, 0, 1);

void des_thrusters::step(double delta)
{
    bool softkilled;
    shm_get(switches, soft_kill, softkilled);
    shm_getg(settings_control, shm_settings);

    if (softkilled || !shm_settings.enabled) {
        f -= f;
        t -= t;
        return;
    }
    
    // Read PID settings & desires.
    // (would switching to watchers improve performance?)

    SHM_GET_PID(settings_heading, shm_hp, hp)
    SHM_GET_PID(settings_pitch, shm_pp, pp)
    SHM_GET_PID(settings_roll, shm_rp, rp)
    SHM_GET_PID(settings_velx, shm_xp, xp)
    SHM_GET_PID(settings_vely, shm_yp, yp)
    SHM_GET_PID(settings_depth, shm_dp, dp)

    shm_getg(desires, shm_desires);

    // Read current orientation, velocity & position (in the model frame).

    // Orientation quaternion in the model frame.
    Eigen::Quaterniond qm = q * mtob_rq;
    Eigen::Vector3d rph = quat_to_euler(qm);

    // Component of the model quaternion about the z-axis (heading-only rotation).
    Eigen::Quaterniond qh = euler_to_quat(rph[2], 0, 0);

    // Velocity in heading modified world space.
    Eigen::Vector3d vp = qh.conjugate() * v;
    
    // Step the PID loops.

    Eigen::Vector3d desires = quat_to_euler(euler_to_quat(shm_desires.heading * DEG_TO_RAD,
                                                          shm_desires.pitch * DEG_TO_RAD,
                                                          shm_desires.roll * DEG_TO_RAD)) * RAD_TO_DEG;
    
    double ho = hp.step(delta, desires[2], rph[2] * RAD_TO_DEG);
    double po = pp.step(delta, desires[1], rph[1] * RAD_TO_DEG);
    double ro = rp.step(delta, desires[0], rph[0] * RAD_TO_DEG);

    double xo = xp.step(delta, shm_desires.speed, vp[0]);
    double yo = yp.step(delta, shm_desires.sway_speed, vp[1]);
    double zo = dp.step(delta, shm_desires.depth, x[2]);

    // f is in the heading modified world frame.
    // t is in the model frame.
    // We will work with f and b in the model frame until the end of this
    // function.

    f[0] = shm_settings.velx_active ? xo : 0;
    f[1] = shm_settings.vely_active ? yo : 0;
    f[2] = shm_settings.depth_active ? zo : 0;

    f *= m;

    Eigen::Vector3d w_in;
    w_in[0] = shm_settings.roll_active ? ro : 0;
    w_in[1] = shm_settings.pitch_active ? po : 0;
    w_in[2] = shm_settings.heading_active ? ho : 0;

    // TODO Avoid this roundabout conversion from hpr frame
    // to world frame and back to model frame.
    Eigen::Quaterniond qhp = euler_to_quat(rph[2], rph[1], 0);
    Eigen::Vector3d w = qm.conjugate() * (Eigen::Vector3d(0, 0, w_in[2]) +
                                     qh * Eigen::Vector3d(0, w_in[1], 0) +
                                    qhp * Eigen::Vector3d(w_in[0], 0, 0));

    t = btom_rm * q.conjugate() * I * q * mtob_rm * w;

    // Output diagnostic information. Shown by auv-control-helm.

    SHM_PUT_PID(control_internal_heading, ho)
    SHM_PUT_PID(control_internal_pitch, po)
    SHM_PUT_PID(control_internal_roll, ro)
    SHM_PUT_PID(control_internal_velx, xo)
    SHM_PUT_PID(control_internal_vely, yo)
    SHM_PUT_PID(control_internal_depth, zo)

    // Subtract passive forces.
    // (this implementation does not support discrimination!)
    if (shm_settings.buoyancy_forces || shm_settings.drag_forces) {
        // pff is in the body frame.
        screw ps = pff();
        f -= qh.conjugate() * q * ps.first;
        t -= btom_rm * ps.second;
    }

    // Hyper-ellipsoid clamping. Sort of limiting to the maximum amount of
    // energy the sub can output (e.g. can't move forward at full speed
    // and pitch at full speed at the same time).
    // Doesn't really account for real-world thruster configurations (e.g.
    // it might be possible to move forward at full speed and ascend at
    // full speed at the same time) but it's just an approximation.
    for (int i = 0; i < 3; i++) {
        f[i] /= axes[i];
    }

    for (int i = 0; i < 3; i++) {
        t[i] /= axes[3 + i];
    }

    double m = f.dot(f) + t.dot(t);
    if (m > 1) {
        double sm = sqrt(m);
        f /= sm;
        t /= sm;
    }

    for (int i = 0; i < 3; i++) {
        f[i] *= axes[i];
    }

    for (int i = 0; i < 3; i++) {
        t[i] *= axes[3 + i];
    }

    // Regular min/max clamping.
    clamp(f, fa, fb, 3);
    clamp(t, ta, tb, 3);

    f = q.conjugate() * qh * f;
    t = mtob_rm * t;
}

} // namespace fishbowl
} // namespace cuauv
