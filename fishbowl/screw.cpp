#include "screw.hpp"

namespace cuauv {
namespace fishbowl {

screw::screw(Eigen::Vector3d f, Eigen::Vector3d t)
    : std::pair<Eigen::Vector3d, Eigen::Vector3d>(f, t)
{
}

screw& operator+=(screw& a, const screw& b)
{
    a.first += b.first;
    a.second += b.second;
    return a;
}

screw& operator-=(screw& a, const screw& b)
{
    a.first -= b.first;
    a.second -= b.second;
    return a;
}

screw operator+(screw a, const screw& b)
{
    return a += b;
}

screw operator-(screw a, const screw& b)
{
    return a -= b;
}

screw operator*(const Eigen::Quaterniond& q, const screw& s)
{
   return screw(q * s.first, q * s.second);
}

screw operator*(const Eigen::Matrix3d& m, const screw& s)
{
   return screw(m * s.first, m * s.second);
}

std::ostream& operator<<(std::ostream& os, const screw& s)
{
    os << "(" << s.first[0] << ", " << s.first[1] << ", " << s.first[2] << ", "
        << s.second[0] << ", " << s.second[1] << ", " << s.second[2] << ")";
    return os;
}

} // namespace fishbowl
} // namespace cuauv
