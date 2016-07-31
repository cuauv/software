#include <Eigen/Core>
#include <Eigen/Geometry>

#include <libshm/c/shm.h>

#include "passive.hpp"
#include "world.hpp"
#include "engine.hpp"
#include "force.hpp"

namespace cuauv {
namespace fishbowl {

double clamp(double x, double a, double b) {
    return std::min(std::max(x, a), b);
}

Eigen::Vector3d clamp(const Eigen::Vector3d& x, const Eigen::Vector3d& a, const Eigen::Vector3d& b)
{
    return Eigen::Vector3d(clamp(x[0], a[0], b[0]), clamp(x[1], a[1], b[1]), clamp(x[2], a[2], b[2]));
}

gravity::gravity(world& w)
    : w(w)
{
}

screw gravity::on(entity_id id)
{
    return screw(Eigen::Vector3d(0, 0, w.get_entity(id).get_m() * 9.81), Eigen::Vector3d(0, 0, 0));
}

void gravity::step(double delta)
{
}

turbulence::turbulence(const Eigen::Vector3d& fa, const Eigen::Vector3d& fb, std::uniform_real_distribution<double> fwd, const Eigen::Vector3d& ta, const Eigen::Vector3d& tb, std::uniform_real_distribution<double> twd)
    : fa(fa)
    , fb(fb)
    , fwd(fwd)
    , ta(ta)
    , tb(tb)
    , twd(twd)
    , f(0, 0, 0)
    , t(0, 0, 0)
{
}

screw turbulence::on(entity_id id)
{
    return screw(f, t);
}

void turbulence::step(double delta)
{
    f += Eigen::Vector3d(fwd(gen), fwd(gen), fwd(gen)) * delta;
    t += Eigen::Vector3d(twd(gen), twd(gen), twd(gen)) * delta;
    f = clamp(f, fa, fb);
    t = clamp(t, ta, tb);
}

buoyancy::buoyancy(const Eigen::Quaterniond& q, double s, const Eigen::Vector3d& x)
    : q(q)
    , s(s)
    , x(x)
{
}

screw buoyancy::on()
{
    const Eigen::Vector3d f = q.conjugate() * Eigen::Vector3d(0, 0, -s);
    const Eigen::Vector3d t(x.cross(f));

    return screw(f, t);
}

void buoyancy::step(double delta)
{
}

drag::drag(const Eigen::Quaterniond& q, const Eigen::Vector3d& v, const Eigen::Vector3d& w, const Eigen::Vector3d& x, const Eigen::Vector3d& n, double c, double a)
    : q(q)
    , v(v)
    , w(w)
    , x(x)
    , n(n)
    , t(x.cross(n))
    , c(c)
    , a(a)
{
}

screw drag::on()
{
    const double vs = (q.conjugate() * v + w.cross(x)).dot(n);
    // Fd = .5 * p * v^2 * c * a
    // assume p (mass density) of pool water is 1 g/cm^3
    // though who knows what might be in there...
    // the force should be opposite the direction of velocity
    double f = (vs > 0 ? -1 : 1) * 0.5 * 1000 * pow(vs, 2) * c * a;
    return screw(n * f, t * f);
}

void drag::step(double delta)
{
}

} // namespace fishbowl
} // namespace cuauv
