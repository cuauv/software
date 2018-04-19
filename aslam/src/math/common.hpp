#pragma once

#define __USE_MATH_DEFINES

#include <cmath>
#include <tuple>
#include <random>
#include <vector>
#include <numeric>
#include <algorithm>

#include <Eigen/Dense>
#include <Eigen/StdVector>

#include <auvlog/client.h>

#define AUVLOG(m) auvlog_log_stdout("aslam", m)

namespace e = Eigen;
namespace s = std;

typedef e::Matrix<double, 6, 1> Vec6;
typedef e::Matrix<double, 6, 6> Mat6;
typedef e::Matrix<double, 3, 1> Vec3;
typedef e::Matrix<double, 3, 3> Mat3;
typedef e::Matrix<double, 2, 1> Vec2;
typedef e::Matrix<double, 2, 2> Mat2;
typedef e::Matrix<double, 4, 1> Vec4;
typedef e::Matrix<double, 4, 4> Mat4;
typedef e::Matrix<double, 8, 1> Vec8;

EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(Vec6)
EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(s::pair<Mat3, double>)
EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(Mat3)
EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(Vec3)
EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(Vec4)
EIGEN_DEFINE_STL_VECTOR_SPECIALIZATION(Mat4)

s::default_random_engine rng;
s::normal_distribution<double> distr (0.0, 1.0);

inline double heading_diff(double x, double y) {
  double diff = fmod(x - y, 2 * M_PI);
  return diff > M_PI ? diff - (2 * M_PI) : diff;
};

template<typename A, typename B>
Vec3 toHPD(const A& pose, const B& pos) {
  Vec3 del (pos[0] - pose[0], pos[1] - pose[1], pos[2] - pose[2]);
  Vec2 ray (del[0], del[1]);
  Vec3 exp (
    heading_diff(atan2(del[1], del[0]), pose[3]),
    heading_diff(atan2(-del[2], ray.norm()), pose[4]),
    del.norm()
  );
  return exp;
};

Vec6 offsetToWorld(const Vec6& sub, const Vec3& offset) {
  Vec6 ret = sub;
  ret[0] += (offset[0] * cos(sub[3])) - (offset[1] * sin(sub[3]));
  ret[1] += (offset[0] * sin(sub[3])) + (offset[1] * cos(sub[3]));
  ret[2] += offset[2];
  return ret;
};

Vec3 rotateXY(const Vec3& vec, double ang) {
  Vec3 ret;
  ret[0] = (vec[0] * cos(ang)) - (vec[1] * sin(ang));
  ret[1] = (vec[0] * sin(ang)) + (vec[1] * cos(ang));
  ret[2] = vec[2];
  return ret;
}

Vec3 offsetFromWorld(const Vec6& sub, const Vec3& offset) {
  Vec3 delta (offset[0] - sub[0], offset[1] - sub[1], offset[2] - sub[2]);
  return rotateXY(delta, -sub[3]);  
};

inline double gaussian(double x, double mu, double sigma) {
  return s::exp(-s::pow(x - mu, 2) / (2 * s::pow(sigma, 2)));
};

template<typename X, typename Y>
inline double multivariate_gaussian(const X& x, const X& mu, const Y& sigma) {
  // http://cs229.stanford.edu/section/gaussians.pdf
  auto v = - ((x - mu).transpose() * sigma.inverse()) * (x - mu);
  return s::exp(v[0] / 2); 
};

template<typename X>
inline double multivariate_gaussian_integral(const X& sigma) {
  // https://en.wikipedia.org/wiki/Gaussian_integral
  return sqrt( s::pow(2 * M_PI, sigma.rows()) / sigma.determinant());
}

template<typename X, typename Y>
inline X multivariate_gauss(const X& mean, const Y& covariance) {
  X rnd = X::Zero();
  //double min = -1.0;
  //double max = 1.0;
  for (unsigned int i = 0; i < mean.size(); i++) {
    //rnd[i] = min + ((double) (rand() / (double) RAND_MAX) * (max - min));
    rnd[i] = distr(rng);
  };
  return (covariance * rnd) + mean;
};

template<typename T>
s::vector<T> scan_sum(s::vector<T> vec) {
  T sum = 0;
  s::vector<T> ret;
  s::for_each(
    vec.begin(),
    vec.end(),
    [&](T& x) {
      sum += x;
      ret.push_back(sum);
    }   
  );  
  return ret;
};

template<typename A, typename F>
A frequency_resample_transform (const A& vec, F function) {
  unsigned int size = vec.size();
  s::vector<double> weights;
  for (unsigned int i = 0; i < size; i++) weights.push_back(vec[i].second);
  s::vector<double> cumulative = scan_sum(weights);
  s::vector<int> selected;
  for (unsigned int ind = 0; ind < weights.size(); ind++) {
    double threshold = static_cast<double>(ind) / static_cast<double>(weights.size());
    for (unsigned int wind = 0; wind < cumulative.size(); wind ++) {
      if (cumulative[wind] > threshold) {
        selected.push_back(wind);
        break;
      };  
    };
  };
  assert(selected.size() == size);
  A ret;
  s::for_each(
    selected.begin(),
    selected.end(),
    [&](int index) {
      ret.push_back(s::make_pair(
        function(vec[index].first),
        vec[index].second
      ));
    }
  ); 
  return ret; 
};

double average_circle(s::vector<double> angles) {
  double x = s::accumulate(angles.begin(), angles.end(), 0.0, [](double x, double y) { return x + cos(y); });
  double y = s::accumulate(angles.begin(), angles.end(), 0.0, [](double x, double y) { return x + sin(y); });
  return fmod(atan2(y, x), 2 * M_PI);
};
