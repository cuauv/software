#include <math.h>
#include <iostream>

#include <Eigen/Core>
#include <Eigen/Geometry>

#include "geometry.hpp"

namespace cuauv {
namespace fishbowl {

const double singularity_cutoff = M_PI/2 * 0.985;

// Sphere-sphere Sweep implementation from:
// http://www.gamasutra.com/view/feature/131790/simple_intersection_tests_for_games.php?page=2

bool quadratic(double a, double b, double c, double& r1, double& r2)
{
    double q = b * b - 4 * a * c;
    if (q < 0) {
        return false;
    } else {
        double sq = sqrt(q);
        double d = 1 / (2 * a);
        r1 = (-b + sq) * d;
        r2 = (-b - sq) * d;
        return true;
    }
}

bool sphere_sphere_sweep(double ar, const Eigen::Vector3d& ax0, const Eigen::Vector3d& ax1,
                         double br, const Eigen::Vector3d& bx0, const Eigen::Vector3d& bx1,
                         double& t0, double& t1)
{
    Eigen::Vector3d av = ax1 - ax0;
    Eigen::Vector3d bv = bx1 - bx0;
    Eigen::Vector3d ab = bx0 - ax0;
    Eigen::Vector3d abv = bv - av;
    double abr = ar + br;
    double a = abv.dot(abv);
    double b = 2 * abv.dot(ab);
    double c = ab.dot(ab) - abr * abr;

    if (ab.dot(ab) <= abr * abr) {
        t0 = 0;
        t1 = 0;
        return true;
    }

    if (quadratic(a, b, c, t0, t1)) {
        if (t0 > t1) {
            double x = t0;
            t0 = t1;
            t1 = x;
        }
        // t0 <= t1
        return (0 <= t0 && t0 <= 1) || (t0 <= 0 && t1 >= 0);
    }

    return false;
}

double line_distance(const Eigen::Vector3d& x0, const Eigen::Vector3d& x1, const Eigen::Vector3d& x)
{
    const Eigen::Vector3d p = x1 - x0;
    const Eigen::Vector3d y = x - x0;
    const Eigen::Vector3d o = (y.dot(p) / p.dot(p)) * p;
    const Eigen::Vector3d r = y - o;

    return r.dot(r);
}

void swing_twist(const Eigen::Quaterniond& q, const Eigen::Vector3d& vt,
                 Eigen::Quaterniond& swing, Eigen::Quaterniond& twist) {
    Eigen::Vector3d p = vt * (q.x() * vt[0] + q.y() * vt[1] + q.z() * vt[2]);
    twist = Eigen::Quaterniond(q.w(), p[0], p[1], p[2]);
    twist.normalize();
    swing = q * twist.conjugate();
}


// Behavior is undefined if q is not a unit quaternion.
// uses body 3-2-1 convention (z, y, x; heading, pitch, roll)
// see: NASA Mission Planning and Analysis Division. "Euler Angles, quaternions, and transformation matrices".
// also see: http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/
// their definitions of 'heading, attitude, and bank' are different from ours, but the equations still work
// because their coordinate axes are just our coordinate axes rotated a bit, i.e., the relationships between
// the axes are the same. Their heading = our pitch [y], attitude = heading [z], bank = roll [x].
// euler[0] is the angle of rotation in radians around the x-axis, in the range [-pi, pi].
// euler[1] " y-axis, in the range [-pi/2, pi/2]
// euler[2] " z-axis, in the range [-pi, pi]
Eigen::Vector3d quat_to_euler(Eigen::Quaterniond q)
{
    Eigen::Vector3d euler;

    // [q0 q1 q2 q3] is in w, x, y, z order
    const double q0 = q.w();
    const double q1 = q.x();
    const double q2 = q.y();
    const double q3 = q.z();

    euler[0] = atan2(2*(q0*q1 + q2*q3), 1-2*(q1*q1 + q2*q2));
    euler[1] = asin(2*(q0*q2 - q3*q1));
    euler[2] = atan2(2*(q0*q3 + q1*q2), 1-2*(q2*q2 + q3*q3));

    // Tentatively handle singularities.
    if (euler[1] > singularity_cutoff || euler[1] < -singularity_cutoff) {
        euler[0] = atan2(q3, q0);
        euler[2] = 0;
    }

    // XX this makes the controller angry
    //euler[0] = fmod(euler[0] + 2*M_PI, 2*M_PI);
    //euler[1] = fmod(euler[1] + 2*M_PI, 2*M_PI);
    //euler[2] = fmod(euler[2] + 2*M_PI, 2*M_PI);

    return euler;
}

// body 3-2-1
Eigen::Quaterniond euler_to_quat(double h, double p, double r)
{
    return Eigen::Quaterniond(Eigen::AngleAxisd(h, Eigen::Vector3d::UnitZ())
                            * Eigen::AngleAxisd(p, Eigen::Vector3d::UnitY())
                            * Eigen::AngleAxisd(r, Eigen::Vector3d::UnitX()));
}

} // namespace fishbowl
} // namespace cuauv
