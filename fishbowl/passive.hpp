#ifndef CUAUV_SIM_PASSIVE_H
#define CUAUV_SIM_PASSIVE_H

#include <Eigen/Core>
#include <Eigen/Geometry>
#include <random>

#include <libshm/c/shm.h>

#include "world.hpp"
#include "engine.hpp"
#include "force.hpp"

namespace cuauv {
namespace fishbowl {

class gravity : public force {
public:
    gravity(world& w);
    screw on(entity_id id);
    void step(double delta);

private:
    world& w;
};

// bounded random walk
class turbulence : public force {
public:
    turbulence(const Eigen::Vector3d& fa, const Eigen::Vector3d& fb, std::uniform_real_distribution<double> fwd, const Eigen::Vector3d& ta, const Eigen::Vector3d& tb, std::uniform_real_distribution<double> twd);
    screw on(entity_id id);
    void step(double delta);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
private:
    std::default_random_engine gen;

    Eigen::Vector3d fa;
    Eigen::Vector3d fb;
    std::uniform_real_distribution<double> fwd;

    Eigen::Vector3d ta;
    Eigen::Vector3d tb;
    std::uniform_real_distribution<double> twd;

    Eigen::Vector3d f;
    Eigen::Vector3d t;
};

class buoyancy : public engine {
public:
    buoyancy(const Eigen::Quaterniond& q, double s, const Eigen::Vector3d& x);
    screw on();
    void step(double delta);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    const Eigen::Quaterniond& q;
    double s;
    Eigen::Vector3d x;
};

class drag : public engine {
public:
    drag(const Eigen::Quaterniond& q, const Eigen::Vector3d& v, const Eigen::Vector3d& w, const Eigen::Vector3d& x, const Eigen::Vector3d& n, double c, double a);
    screw on();
    void step(double delta);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    const Eigen::Quaterniond& q;
    const Eigen::Vector3d& v;
    const Eigen::Vector3d& w;
    Eigen::Vector3d x;
    Eigen::Vector3d n;
    Eigen::Vector3d t;
    double c;
    double a;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_PASSIVE_H
