#ifndef CUAUV_SIM_VISION_H
#define CUAUV_SIM_VISION_H

#include <unordered_set>
#include <utility>

#include <Eigen/Core>

#include "world.hpp"

namespace cuauv {
namespace fishbowl {

class camera  {
public:
    /**
     * Constructs a new selective attached pinhole camera.
     * @param w The world.
     * @param pq A pointer to the orientation quaternion of the parent object.
     * @param px A pointer to the position of the parent object.
     * @param q The orientation of the camera in the praent object frame.
     * @param x The position of the camera in the parent object frame.
     * @param f The focal length of the camera.
     */
    camera(world* w, Eigen::Quaterniond* pq, Eigen::Vector3d* px, const Eigen::Quaterniond& q, const Eigen::Vector3d& x, double f);

    Eigen::Quaterniond* pq;
    Eigen::Vector3d* px;
    Eigen::Quaterniond q;
    Eigen::Vector3d x;
    double f;

    void step();

    /**
     * @returns A tuple of the form (x, y, r, d).  (x, y) corresponds to the
     * apparent position of the target entity, r is the apparent radius of the
     * target entity, and d is the distance from the camera to the target
     * entity.  The origin is along the camera's x axis. The x values increase
     * to the right, and y values increase downwards.
     */
    std::tuple<double, double, double, double> query(entity_id id);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

private:
    world* w;

    Eigen::Matrix<double, 3, 4> C;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_VISION_H
