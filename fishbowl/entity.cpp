#include <Eigen/Core>
#include <Eigen/Dense>
#include <Eigen/Geometry>

#include "entity.hpp"

namespace cuauv {
namespace fishbowl {

entity::entity(double m, double r, const inertia_tensor& I, const Eigen::Quaterniond& btom_rq)
    : m(m)
    , r(r)
    , I(I)
    , btom_rq(btom_rq)
    , btom_rm(btom_rq.matrix())
    , mtob_rm(btom_rq.conjugate().matrix())
{
    if (m <= 0) throw std::invalid_argument("expected m > 0.");
    if (r <= 0) throw std::invalid_argument("expected r > 0.");

    Eigen::Vector3d diag = I.diagonal();
    if (diag[0] == 0 || diag[1] == 0 || diag[2] == 0) throw std::invalid_argument("expected I fully non-zero");

    Eigen::Vector3d& diagr = Ir.diagonal();
    for (int i = 0; i < 3; i++)
        diagr[i] = 1.0 / diag[i];
}

double entity::get_m() const { return m; }
double entity::get_r() const { return r; }

inertia_tensor entity::get_I() const { return I; }
inertia_tensor entity::get_Ir() const { return Ir; }

Eigen::Matrix3d entity::get_btom_rm() const { return btom_rm; }
Eigen::Matrix3d entity::get_mtob_rm() const { return mtob_rm; }
Eigen::Quaterniond entity::get_model_q() const { return Eigen::Quaterniond(q * btom_rq.conjugate()).normalized(); }

} // namespace fishbowl
} // namespace cuauv
