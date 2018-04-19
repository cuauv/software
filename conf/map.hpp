#pragma once

#include <string>
#include <vector>

#include <Eigen/Dense>

namespace cuauv {
namespace conf {

namespace e = Eigen;
namespace s = std;

struct object {
  s::string name;
  e::Matrix<double, 4, 1> initial_position;
  e::Matrix<double, 4, 4> initial_covariance;
};

struct sub {
  e::Matrix<double, 6, 1> initial_pose;
};

struct map {
//  sub submarine;
  s::vector<object> objects;
  int num_particles;
};

/** Loads map from CUAUV_LOCALE environment variable. 
 * @throws s::runtime_error if file cannot be read.
 **/
map load_map(void);

}
}
