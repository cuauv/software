#ifndef CUAUV_SIM_SCREW_H
#define CUAUV_SIM_SCREW_H

#include <iostream>
#include <utility>

#include <Eigen/Core>
#include <Eigen/Geometry>

namespace cuauv {
namespace fishbowl {

class screw : public std::pair<Eigen::Vector3d, Eigen::Vector3d> {
public:
    screw(Eigen::Vector3d f, Eigen::Vector3d t);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

screw& operator+=(screw& a, const screw& b);
screw& operator-=(screw& a, const screw& b);

screw operator+(const screw& a, const screw& b);
screw operator-(const screw& a, const screw& b);

screw operator*(const Eigen::Quaterniond& q, const screw& s);
screw operator*(const Eigen::Matrix3d& m, const screw& s);

std::ostream& operator<<(std::ostream& os, const screw& s);

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_SCREW_H
