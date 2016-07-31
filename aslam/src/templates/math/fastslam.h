#pragma once

#define _USE_MATH_DEFINES

#include <map>
#include <mutex>
#include <string>
#include <numeric>
#include <iostream>

// C++ templates, sheesh - http://stackoverflow.com/questions/495021/why-can-templates-only-be-implemented-in-the-header-file
#include "pointcloud.hpp"

class Particle {
  public:
    Particle(const $!sub['state_type']!$& initial_pose);
    Particle(const Particle& other);
    ~Particle();

    $!sub['state_type']!$ pose;
<!--(for o in objects)-->
    PointCloud<$!o['state_type']!$, $!o['covariance_type']!$> $!o['name']!$;
<!--(end)-->

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

class Map {
  public:
    Map(const $!sub['state_type']!$& initial_pose);

<!--(for o in objects)-->
    template<typename A, typename B>
    void apply_$!o['name']!$(A update, B prior);
<!--(end)-->

    /* 
      Iterate over particles, apply current motion prediction (velocity * dt). 
    */
    void step_controls(const Vec6& rate, const Mat6& covariance, double dt); // Ref 1. [6]
 
    /* 
      Integrate over possible object positions, multiply previous prior by probability given observation.
       Apparently this is closed-form for a Gaussian.
    */
    void step_resample(); // Ref 1. [7]

    void estimate();
    
    $!sub['state_type']!$ combined_sub;
<!--(for o in objects)-->
    PointCloud<$!o['state_type']!$, $!o['covariance_type']!$> combined_$!o['name']!$;
    $!o['covariance_type']!$ combined_intrinsic_covariance_$!o['name']!$;
<!--(end)-->

    s::vector<s::pair<Particle, double>> particles; 
    s::mutex particle_mutex;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};
