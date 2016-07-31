#ifndef CUAUV_CONF_SIMULATOR_H
#define CUAUV_CONF_SIMULATOR_H

#include <random>

#include <Eigen/Core>

namespace cuauv {
namespace conf {

struct simulator {
};

simulator load_simulator(const std::string& filename);

} // namespace conf
} // namespace cuauv

#endif // CUAUV_CONF_SIMULATOR_H
