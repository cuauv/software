#ifndef CUAUV_SIM_ENTITY_H
#define CUAUV_SIM_ENTITY_H

#include <Eigen/Core>
#include <Eigen/Geometry>

namespace cuauv {
namespace fishbowl {

typedef Eigen::DiagonalMatrix<double, 3> inertia_tensor;

/**
 * Entity represents an entity in a World.
 */
class entity {
public:
    /**
     * Constructs a new entity.
     *
     * @param m Mass.
     * @param r Radius.
     * @param I Inertia tensor, in the body frame.
     * @param btom_rq Body to model frame rotation quaternion. Undefined behavior if not normalized.
     */
    entity(double m, double r, const inertia_tensor& I, const Eigen::Quaterniond& btom_rq);

    double get_m() const; //!< Mass.
    double get_r() const; //!< Radius.
    inertia_tensor get_I() const; //!< Inertia tensor, in the body frame.

    Eigen::Vector3d x { 0, 0, 0 }; //!< Position.
    Eigen::Vector3d v { 0, 0, 0 }; //!< Translational velocity, in the world frame.
    Eigen::Vector3d a { 0, 0, 0 }; //!< Translational acceleration in the previous timestep, in the world frame.

    Eigen::Vector3d xp { 0, 0, 0 }; //!< Position, in the previous timestep. Initially 0.

    /**
     * Orientation, of the body relative to the world frame.
     * To convert to the orientation of the model relative to the world frame, use
     * <tt>q * body_rm_inv</tt>
     * Behavior is undefined if q is not a unit quaternion.
     */
    Eigen::Quaterniond q { 1, 0, 0, 0 };
    Eigen::Vector3d w { 0, 0, 0 }; //!< Angular velocity, in the body frame.
    Eigen::Vector3d t { 0, 0, 0 }; //<! Torque, in the body frame, in the previous timestep.

    bool corporeal = true; //!< True iff forces and engines should affect this entity.

    /**
     * The rotation matrix from the body frame to the model frame.
     * We precompute the matrix because it is more efficient for rotation
     * operations, according to Eigen's documentation.
     * @see http://eigen.tuxfamily.org/dox/classEigen_1_1Quaternion.html
     */
    Eigen::Matrix3d get_btom_rm() const;
    Eigen::Matrix3d get_mtob_rm() const;

    // Utility functions.

    inertia_tensor get_Ir() const; //!< The reciprocal of the inertia tensor, in the body frame.
    Eigen::Quaterniond get_model_q() const; //!< Orientation, of the model relative to the world frame.

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    double m;
    double r;
    inertia_tensor I;
    inertia_tensor Ir;
    Eigen::Quaterniond btom_rq;
    Eigen::Matrix3d btom_rm;
    Eigen::Matrix3d mtob_rm;
    Eigen::Quaterniond model_q;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_ENTITY_H
