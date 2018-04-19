#pragma once

#include "common.hpp"
#include "../config/constants.h"

template<typename X, typename Y>
class PointCloud {
 private:
    void resample();
 public:
    PointCloud();
    PointCloud(X initial_state, Y initial_covariance);
    PointCloud(const PointCloud<X, Y>& other);

    template<typename A>
    void update(A prior);

    void guard();
    void normalize();

    X mean() const;
    Y covariance() const;

    Y intrinsic_covariance;

    s::vector<s::pair<X, double>> particles;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template<typename X, typename Y>
PointCloud<X, Y>::PointCloud() {
  this->intrinsic_covariance = Y::Zero();
}

template<typename X, typename Y>
PointCloud<X, Y>::PointCloud(X initial_state, Y initial_covariance) {
  this->particles.reserve(NUM_PARTICLES_PER_OBJECT_MAP);
  for (unsigned int i = 0; i < NUM_PARTICLES_PER_OBJECT_MAP; i++) {
    this->particles.push_back(s::make_pair(multivariate_gauss(initial_state, initial_covariance), 1.0 / NUM_PARTICLES_PER_OBJECT_MAP));
  }
  this->intrinsic_covariance = initial_covariance;
};

template<typename X, typename Y>
PointCloud<X, Y>::PointCloud(const PointCloud& other) {
  this->particles = s::vector<s::pair<X, double>> (other.particles);
  this->intrinsic_covariance = other.intrinsic_covariance;
};

template<typename X, typename Y>
template<typename A>
void PointCloud<X, Y>::update(A prior) {
  this->guard();
  for (unsigned int i = 0; i < this->particles.size(); i++) {
    this->particles[i].second *= prior(this->particles[i].first);
  }; 
  this->normalize();
  this->resample();
  this->normalize();
};

template<typename X, typename Y>
void PointCloud<X, Y>::guard() {
  unsigned int num = this->particles.size() * (1 - FRACTION_ANYWHERE);
  Y anywhere = Y::Zero();
  for (unsigned int i = 0; i < this->particles[0].first.rows(); i++) {
    if (i == 0 || i == 1) anywhere(i, i) = 20.0;
    if (i == 2) anywhere(i, i) = 5.0;
    if (i == 3) anywhere(i, i) = 2 * M_PI;
  };
  for (unsigned int i = num; i < this->particles.size(); i++) {
    this->particles[i].first = multivariate_gauss(this->particles[i].first, anywhere);
    this->particles[i].second = 1 / NUM_PARTICLES_PER_OBJECT_MAP;
  }
}

template<typename X, typename Y>
void PointCloud<X, Y>::resample() {
  Y cov = Y::Zero();
  this->intrinsic_covariance = this->covariance();
  double val = s::max(0.01, s::min(this->intrinsic_covariance.norm() / 2, 1.0));
  for (unsigned int i = 0; i < cov.rows(); i++) cov(i, i) = val;
  this->particles = frequency_resample_transform(this->particles, [&](const auto& val) { return multivariate_gauss(val, cov); }); 
};

template<typename X, typename Y>
void PointCloud<X, Y>::normalize() {
  double sum = 0;
  for (unsigned int i = 0; i < this->particles.size(); i++) {
    sum += this->particles[i].second;
  };
  for (unsigned int i = 0; i < this->particles.size(); i++) {
    this->particles[i].second /= sum;
  };
};

template<typename X, typename Y>
X PointCloud<X, Y>::mean() const {
  X sum = X::Zero();
  for (unsigned int i = 0; i < this->particles.size(); i++) {
    sum += this->particles[i].first * this->particles[i].second;
  }
  return sum;
};

template<typename X, typename Y>
Y PointCloud<X, Y>::covariance() const {
  X mean = this->mean();
  Y cov = Y::Zero();
  for (int i = 0; i < this->particles[0].first.rows(); i++) {
    for (int j = 0; j < this->particles[0].first.rows(); j++) {
      for (unsigned int k = 0; k < this->particles.size(); k++) {
        cov(i, j) += (this->particles[k].first[i] - mean[i]) * (this->particles[k].first[j] - mean[j]) * this->particles[k].second;
      }   
    }   
  }
  return cov;
};
