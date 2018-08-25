#include <string>
#include <Eigen/Dense>

typedef Eigen::Matrix<float, 3, 1> vec3;
typedef Eigen::Matrix<float, 3, 3> mat3;
typedef Eigen::Matrix<float, 6, 1> vec6;
typedef Eigen::Matrix<float, Eigen::Dynamic, 1> vecX;

struct Observation {
    std::string id;
    int m_x;
    int m_y;
    int m_z;
    int u_x;
    int u_y;
    int u_z;
};
