#include <unordered_set>
#include <tuple>

#include <Eigen/Core>

#include "vision.hpp"
#include "world.hpp"

namespace cuauv {
namespace fishbowl {

camera::camera(world* w, Eigen::Quaterniond* pq, Eigen::Vector3d* px, const Eigen::Quaterniond& q, const Eigen::Vector3d& x, double f)
    : pq(pq)
    , px(px)
    , q(q)
    , x(x)
    , f(f)
    , w(w)
    , C(Eigen::Matrix<double, 3, 4>::Zero())
{
    step();
}

void camera::step()
{
    C.block<3, 3>(0, 0) = (q.conjugate() * pq->conjugate()).matrix();
    C.col(3) = C.block<3, 3>(0, 0) * (-(*px) - ((*pq) * x));
}

std::tuple<double, double, double, double> camera::query(entity_id id)
{
    const entity& e = w->get_entity(id);
    const Eigen::Vector3d tx = e.x;
    // target position homogenous
    Eigen::Vector4d txh(tx[0], tx[1], tx[2], 1);
    // target position in the camera frame
    Eigen::Vector3d txc = C * txh;

    if (txc[0] == 0) {
        return std::make_tuple(0, 0, 0, 0);
    }

    txc[1] *= f/txc[0];
    txc[2] *= f/txc[0];

    return std::make_tuple(txc[1], txc[2], txc[0] > 0 ? e.get_r() * f/txc[0] : 0, txc[0]);
}

} // namespace fishbowl
} // namespace cuauv
