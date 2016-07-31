#ifndef CUAUV_SIM_GEOMETRY_H
#define CUAUV_SIM_GEOMETRY_H

#include <Eigen/Core>
#include <Eigen/Geometry>

namespace cuauv {
namespace fishbowl {

/**
 * sphere_sphere_sweep calculates the normalized first and second times of
 * intersection of two spheres. The times are expressed in terms of the given
 * positions, and are as if the distances traveled by the spheres were covered
 * in unit time.
 * @param ar Radius of the first sphere.
 * @param ax0 Previous position of the first sphere.
 * @param ax1 Current position of the first sphere.
 * @param br Radius of the second sphere.
 * @param bx0 Previous position of the second sphere.
 * @param bx1 Current position of the second sphere.
 * @param t0 Will hold the (normalized) time of the first intersection, if any.
 *           If no intersection occurs, the value is undefined.
 * @param t1 Will hold the time of the second intersection.
 * @returns True iff an intersection occurs.
 * @see http://www.gamasutra.com/view/feature/131790/simple_intersection_tests_for_games.php?page=2
 */
bool sphere_sphere_sweep(double ar, const Eigen::Vector3d& ax0, const Eigen::Vector3d& ax1,
                         double br, const Eigen::Vector3d& bx0, const Eigen::Vector3d& bx1,
                         double& t0, double& t1);

double line_distance(const Eigen::Vector3d& x0, const Eigen::Vector3d& x1, const Eigen::Vector3d& x);

/**
 * swing_twist decomposes a quaternion into swing and twist components.  The
 * swing twist component is a rotation around the twist axis, while the swing
 * component is a rotation around a direction vector perpendicular to the twist
 * axis.
 * The original quaternion q = swing * twist.
 * @param q The quaternion to decompose.
 * @param vt The twist axis.
 * @param swing The swing component of the rotation.
 * @param twist The twist component of the rotation.
 * @see http://www.alinenormoyle.com/weblog/?p=726.
 * @see "Swing-twist decomposition in Clifford algebra" (Dobrowolski, 2015).
 */
void swing_twist(const Eigen::Quaterniond& q, const Eigen::Vector3d& vt,
                 Eigen::Quaterniond& swing, Eigen::Quaterniond& twist);

Eigen::Vector3d quat_to_euler(Eigen::Quaterniond q);
Eigen::Quaterniond euler_to_quat(double h, double p, double r);

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_GEOMETRY_H
