#ifndef CUAUV_CONF_VEHICLE_H
#define CUAUV_CONF_VEHICLE_H

#include <string>
#include <utility>
#include <vector>

#include <Eigen/Core>
#include <Eigen/Geometry>
#include <Eigen/StdVector>

namespace cuauv {
namespace conf {

struct drag_plane {
    Eigen::Vector3d x;
    Eigen::Vector3d n;
    double c;
    double a;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

struct thruster {
    std::string name;
    Eigen::Vector3d x;
    double yaw;
    double pitch;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

struct camera {
    std::string tag;
    std::string type;
    int id;
    std::string camera_name;
    int width;
    int height;
    Eigen::Vector3d position;
    Eigen::Vector3d orientation_hpr;

    // Sensor sizes and focal length are in millimeters.
    double sensor_width;
    double sensor_height;
    double focal_length;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

typedef std::vector<drag_plane, Eigen::aligned_allocator<drag_plane>> drag_plane_vector;
typedef std::vector<thruster, Eigen::aligned_allocator<thruster>> thruster_vector;
typedef std::vector<camera, Eigen::aligned_allocator<camera>> camera_vector;

// Eigen only goes up to Vector4d. Lower-cased because the rest of this code
// uses C++ standard library capitalization.
typedef Eigen::Matrix<double, 6, 1> vector6d;

struct vehicle {
    Eigen::Vector3d center_of_buoyancy;
    double buoyancy_force;
    double gravity_force;
    double sub_height;
    Eigen::Matrix3d I;
    Eigen::Vector3d Ib;
    Eigen::Quaterniond btom_rq;
    drag_plane_vector drag_planes;
    vector6d cwhe_axes;
    vector6d thruster_minimums;
    vector6d thruster_maximums;
    drag_plane_vector uncompensated_drag_planes;
    thruster_vector thrusters;
    camera_vector cameras;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

/**
 * Loads a vehicle object from the TOML file at
 * ${CUAUV_SOFTWARE}/conf/${CUAUV_VEHICLE}.toml
 * @throws std::runtime_error Thrown if the file cannot be opened or parsed.
 */
vehicle load_vehicle(void);

} // namespace conf
} // namespace cuauv

#endif // CUAUV_CONF_VEHICLE_H
