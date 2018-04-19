#include "ekf.h"

template<typename X, typename Y, typename A, typename B, typename C, typename D>
EKF<X, Y, A, B, C, D>::EKF(X initial_state, Y initial_covariance) {
  this->mean       = initial_state;
  this->covariance = initial_covariance;
};

template<typename X, typename Y, typename A, typename B, typename C, typename D>
template<typename E, typename F>
void EKF<X, Y, A, B, C, D>::predict(E transition_func, F transition_jacobian, A control, B noise_covariance) {
  X prev_state      = this->mean;
  Y prev_covariance = this->covariance;
  B jacobian        = transition_jacobian(prev_state);
  this->mean        = transition(prev_state, control);
  this->covariance  = jacobian * prev_covariance * jacobian.transpose() + noise_covariance;
};

template<typename X, typename Y, typename A, typename B, typename C, typename D>
template<typename G, typename H>
void EKF<X, Y, A, B, C, D>::update(G observation_func, H observation_jacobian, C observation, D noise_covariance) {
  X prev_state              = this->mean;
  Y prev_covariance         = this->covariance;
  e::Matrix3d jacobian      = observation_jacobian(prev_state);
  this->residual            = observation - observation_func(prev_state);
  this->residual_covariance = (jacobian * (prev_covariance * jacobian.transpose())) + noise_covariance;
  e::FullPivHouseholderQR<D> decomp(this->residual_covariance); // Should we do M-P pseudoinverse here instead?
  // e::ColPivHouseholderQR<D> decomp(this->residual_covariance); // Should we do M-P pseudoinverse here instead?
  e::Matrix3d inverse       = decomp.inverse();
  e::Matrix3d temp_gain     = (prev_covariance * (jacobian.transpose() * inverse));
  if (s::isfinite(temp_gain.norm())) this->gain = temp_gain; // TODO Fixes numerical instability, but this could be better.
  this->mean                = prev_state + (gain * this->residual);
  this->covariance          = (e::Matrix3d::Identity() - (gain * jacobian)) * prev_covariance;
};


template<typename X, typename Y, typename A, typename B, typename C, typename D>
void EKF<X, Y, A, B, C, D>::covariance_floor(Y covariance_floor) {
  for (int i = 0; i < this->covariance.rows(); i++) {
    for (int j = 0; j < this->covariance.cols(); j++) {
      this->covariance(i, j) = s::max(this->covariance(i, j), covariance_floor(i, j));
    }
  }
}
