#pragma once

#include <vector>
#include <iostream>
#include <algorithm>

#include <Eigen/Dense>

namespace e = Eigen;
namespace s = std;

// State (mean), Covariance, Control, Observation, Jacobian

template<typename X, typename Y, typename A, typename B, typename C, typename D>
class EKF {
  public: 
    EKF(X initial_state, Y initial_covariance);

    template<typename E, typename F>
    void predict(E transition_func, F transition_jacobian, A control, B noise_covariance);
    template<typename G, typename H>
    void update(G observation_func, H observation_jacobian, C observation, D noise_covariance);
    void covariance_floor(Y covariance_floor);
    
    X mean;
    Y covariance; 
  
    C residual;
    D residual_covariance;
    Y gain;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};
