#pragma once

#include "common.hpp"

#include <conf/vehicle.hpp>
#include <auv_math/camera.hpp>

typedef PointCloud<Vec3, Mat3> PC3;
typedef PointCloud<Vec4, Mat4> PC4;

namespace conf = cuauv::conf;
namespace cm   = cuauv::math;

typedef s::function<double(const Vec6&, const Vec3&)> UpdateFunction;
typedef s::function<double(const Vec6&, const PC3&)> PriorFunction;
typedef s::pair<UpdateFunction, PriorFunction> ApplicableObservation;
typedef s::function<double(const Vec6&, const Vec4&)> UpdateFunction4;
typedef s::function<double(const Vec6&, const PC4&)> PriorFunction4;
typedef s::pair<UpdateFunction4, PriorFunction4> ApplicableObservation4;
typedef s::pair<Vec3, Vec3> ObservationLine;

conf::camera lookupCamera(const s::map<s::string, conf::camera> cameras, const s::string tag) {
  auto iter = cameras.find(tag);
  if (iter == cameras.end())
    throw s::runtime_error("Camera not found in vehicle configuration file!");
  return iter->second;  
}

ObservationLine hpdToLine (const Vec6& pose, const Vec3& offset, const Vec3& hpd) {
  Vec6 _pose = offsetToWorld(pose, offset);
  Vec3 _exp (cos(heading_diff(hpd[0], -_pose[3])), sin(heading_diff(hpd[0], -pose[3])), sin(-heading_diff(hpd[1], -pose[4])));
  _exp /= _exp.norm();
  _exp *= hpd[2];
  _exp[0] += _pose[0];
  _exp[1] += _pose[1];
  _exp[2] += _pose[2];
  Vec3 origin (_pose[0], _pose[1], _pose[2]);
  return s::make_pair(origin, _exp);
}

Vec3 xyrToHPD(const conf::camera& camera, int cX, int cY, double radius, double realRadius) {
  double heading, pitch;
  s::tie(heading, pitch)  = cm::calc_forward_angles(camera, cX, cY);
  auto angle_subtended    = cm::calc_angle_subtended(camera, radius, cX);
  double distance         = realRadius / tan(angle_subtended);
  Vec3 obs (heading, pitch, distance);
  return obs;
}

Mat3 xyrToHPDCovariance(double probability) {
  Mat3 cov = Mat3::Zero();
  cov(0, 0) = 0.15 / probability;
  cov(1, 1) = 0.15 / probability;
  cov(2, 2) = 1.5 / probability;
  return cov;
}

Mat3 sonarCovariance(double probability) {
  Mat3 cov = Mat3::Zero();
  cov(0, 0) = 0.1 / probability;
  cov(1, 1) = 0.1 / probability;
  cov(2, 2) = 3.0 / probability; // * distance? do we care about pitch?
  return cov;
};

ApplicableObservation observationFromOffsetHPD(const Vec3& offset, const Vec3& obs, const Mat3& cov, const Vec3& object_offset=Vec3(0, 0, 0)) {
  const auto update = [=](const Vec6& sub_pose, const Vec3& pos) {
    Vec6 pose = offsetToWorld(sub_pose, offset);
    Vec3 exp = toHPD(pose, pos + object_offset);
    double res = multivariate_gaussian(obs, exp, cov);
    return res;
  };
  const auto prior = [=](const Vec6& sub_pose, const PC3& pc) {
    const double depth = hpdToLine(sub_pose, offset, obs).second[2];
    if (depth < MIN_DEPTH || depth > MAX_DEPTH) {
      AUVLOG("Object above water or below floor, rejecting!")
      return -1.0; // Object above water!
    };
    Vec6 pose = offsetToWorld(sub_pose, offset);
    double res = 0.0;
    s::for_each(
      pc.particles.begin(),
      pc.particles.end(),
      [&](const auto& v) {
        Vec3 exp = toHPD(pose, v.first + object_offset);
        res += multivariate_gaussian(obs, exp, cov) * v.second;
      }
    );
    res /= s::abs(multivariate_gaussian_integral(cov));
    return res;
  };
  return s::make_pair(update, prior);
}

ApplicableObservation4 observationFromOffsetOrientationHPD(const Vec3& offset, const Vec3& obs, const Mat3& cov, const Vec3& object_offset=Vec3(0, 0, 0)) {
  const auto update = [=](const Vec6& sub_pose, const Vec4& pos) {
    Vec6 pose = offsetToWorld(sub_pose, offset);
    Vec3 exp = toHPD(pose, Vec3 (pos[0], pos[1], pos[2]) + rotateXY(object_offset, pos[3]) );
    double res = multivariate_gaussian(obs, exp, cov);
    return res;
  };
  const auto prior = [=](const Vec6& sub_pose, const PC4& pc) {
    if (hpdToLine(sub_pose, offset, obs).second[2] < 0.0) {
      AUVLOG("Object above water, rejecting!")
      return -1.0; // Object above water!
    };
    Vec6 pose = offsetToWorld(sub_pose, offset);
    double res = 0.0;
    s::for_each(
      pc.particles.begin(),
      pc.particles.end(),
      [&](const auto& v) {
        Vec3 exp = toHPD(pose, Vec3 ( v.first[0], v.first[1], v.first[2] ) + rotateXY(object_offset, v.first[3]));
        res += multivariate_gaussian(obs, exp, cov) * v.second;
      }
    );
    res /= s::abs(multivariate_gaussian_integral(cov));
    return res;
  };
  return s::make_pair(update, prior);
}

const ApplicableObservation noObservation = s::make_pair(
  [](const Vec6& sub_pose, const Vec3& pos) { return 0.0; },
  [](const Vec6& sub_pose, const PC3& pc) { return -1.0; }
);

const ApplicableObservation4 noObservation4 = s::make_pair(
  [](const Vec6& sub_pose, const Vec4& pos) { return 0.0; },
  [](const Vec6& sub_pose, const PC4& pc) { return -1.0; }
);

template<typename G, typename realRadius>
ApplicableObservation observationFromGenericForwardVision(G group, const s::map<s::string, conf::camera> cameras) {
  if (group.probability > 0) {
    const conf::camera& camera = lookupCamera(cameras, group.camera);
    Vec3 hpd = xyrToHPD(camera, group.center_x, group.center_y, group.radius, static_cast<double>(realRadius::num) / realRadius::den);
    Mat3 cov = xyrToHPDCovariance(group.probability);
    AUVLOG("Received positive vision identification from camera " + camera.tag + " with offset "
      + s::to_string(camera.position[0]) + " N, " + s::to_string(camera.position[1]) + " E, "
      + s::to_string(camera.position[2]) + " D, converted to heading " + s::to_string(hpd[0]) + ", pitch "
      + s::to_string(hpd[1]) + ", distance " + s::to_string(hpd[2]));
    return observationFromOffsetHPD(camera.position, hpd, cov);
  } else {
    return noObservation;
  }
};

template<typename G, typename realRadius>
ApplicableObservation observationFromGenericSonar(G group, const s::map<s::string, conf::camera> cameras) {
  if (group.probability > 0) {
    Vec3 obs = Vec3 ( group.delta_x, group.delta_y, 0 );
    Mat3 cov = sonarCovariance(group.probability);
    const auto update = [=](const Vec6& sub_pose, const Vec3& pos) {
      Vec3 exp = offsetFromWorld(sub_pose, pos);
      return multivariate_gaussian(obs, exp, cov);
    };
    const auto prior = [=](const Vec6& sub_pose, const PC3& pc) {
      double res = 0.0;
      s::for_each(
        pc.particles.begin(),
        pc.particles.end(),
        [&](const auto& v) {
          Vec3 exp = offsetFromWorld(sub_pose, v.first);
          res += multivariate_gaussian(obs, exp, cov) * v.second;
        }
      );
      res /= s::abs(multivariate_gaussian_integral(cov));
      return res;
    };
    AUVLOG("Received positive sonar identification of dX " + s::to_string(group.delta_x) + ", dY " + s::to_string(group.delta_y))
    return s::make_pair(update, prior);
  } else {
    return noObservation;
  }
}

template<typename G, typename realRadius>
ApplicableObservation observationFromGenericForwardVisionArea(G group, const s::map<s::string, conf::camera> cameras) {
  if (group.probability > 0) {
    const conf::camera& camera = lookupCamera(cameras, group.camera);
    Vec3 hpd = xyrToHPD(camera, group.center_x, group.center_y, sqrt (group.area / M_PI), static_cast<double>(realRadius::num) / realRadius::den);
    Mat3 cov = xyrToHPDCovariance(group.probability);
    AUVLOG("Received positive vision identification from camera " + camera.tag + " with offset "
      + s::to_string(camera.position[0]) + " N, " + s::to_string(camera.position[1]) + " E, "
      + s::to_string(camera.position[2]) + " D, converted to heading " + s::to_string(hpd[0]) + ", pitch "
      + s::to_string(hpd[1]) + ", distance " + s::to_string(hpd[2]));
    return observationFromOffsetHPD(camera.position, hpd, cov);
  } else {
    return noObservation;
  }
};

template<typename G, typename realRadius>
ApplicableObservation4 observationFromNavigationGroup(G group, const s::map<s::string, conf::camera> cameras) {
  const conf::camera& camera = lookupCamera(cameras, group.camera);
  auto get_obs_f = [&group, &camera] (int x1, int x2, int y1, int y2, double prob, double area, const Vec3 &offset) {
    if (prob > 0) {
      auto center_x = (x2 + x1) / 2.0;
      auto center_y = (y2 + y1) / 2.0;
      auto length = sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));

      Vec3 hpd = xyrToHPD(camera, center_x, center_y, area / length, static_cast<double>(realRadius::num) / realRadius::den);
      Mat3 cov = xyrToHPDCovariance(group.bottom_prob);

      AUVLOG("Received positive vision identification from camera " + camera.tag + " with offset "
        + s::to_string(camera.position[0]) + " N, " + s::to_string(camera.position[1]) + " E, "
        + s::to_string(camera.position[2]) + " D, converted to heading " + s::to_string(hpd[0]) + ", pitch "
        + s::to_string(hpd[1]) + ", distance " + s::to_string(hpd[2]));

      return observationFromOffsetOrientationHPD(camera.position, hpd, cov, offset);
    }

    return noObservation4;
  };

#define MAKE_OBS(bar_name, offset) \
  auto bar_name##_obs_f = get_obs_f(group.bar_name##_x1, group.bar_name##_x2, group.bar_name##_y1, group.bar_name##_y2,\
                                    group.bar_name##_prob, group.bar_name##_area, offset);\

  MAKE_OBS(bottom, Vec3(0, 0, 0))
  MAKE_OBS(left, Vec3(0, -1, 0))
  MAKE_OBS(right, Vec3(0, 1, 0))
  
  // TODO find the real offsets

  return s::make_pair(
    [=](const Vec6& sub_pose, const Vec4& pos) {
      return bottom_obs_f.first(sub_pose, pos) * left_obs_f.first(sub_pose, pos) * right_obs_f.first(sub_pose, pos);
    },
    [=](const Vec6& sub_pose, const PC4& pc) {
      auto a = bottom_obs_f.second(sub_pose, pc);
      auto b = left_obs_f.second(sub_pose, pc);
      auto c = right_obs_f.second(sub_pose, pc);
      if (a == -1.0 || b == -1.0 || c == -1.0) return -1.0;
      return a * b * c;
    }
  );
};

template<typename G, typename realHeight>
ApplicableObservation4 observationFromTorpedoesGroup(G group, const s::map<s::string, conf::camera> cameras) {
  if (group.board_prob > 0) {
    const conf::camera& camera = lookupCamera(cameras, group.camera);
    Vec3 hpd = xyrToHPD(camera, group.board_center_x, group.board_center_y, group.board_height, static_cast<double>(realHeight::num) / realHeight::den);
    Mat3 cov = xyrToHPDCovariance(group.board_prob);
    AUVLOG("Received positive vision identification from camera " + camera.tag + " with offset "
      + s::to_string(camera.position[0]) + " N, " + s::to_string(camera.position[1]) + " E, "
      + s::to_string(camera.position[2]) + " D, converted to heading " + s::to_string(hpd[0]) + ", pitch "
      + s::to_string(hpd[1]) + ", distance " + s::to_string(hpd[2]));
    return observationFromOffsetOrientationHPD(camera.position, hpd, cov);
  } else {
    return noObservation4;
  }
};

template<typename G, typename D, typename E, typename headingUncertainty, typename R>
R observationFromPipe(G group, const s::map<s::string, conf::camera> cameras) {
  double heading = group.heading;
  double covariance =  static_cast<double> (headingUncertainty::num) / headingUncertainty::den;
  const auto update = [=](const Vec6& sub_pose, const D& pos) {
    Vec3 exp = toHPD(sub_pose, Vec3 (pos[0], pos[1], pos[2]) );
    double res = gaussian(heading, exp[0], covariance);
    return res;
  };
  const auto prior = [=](const Vec6& sub_pose, const E& pc) {
    double res = 0.0;
    s::for_each(
      pc.particles.begin(),
      pc.particles.end(),
      [&](const auto& v) {
        Vec3 exp = toHPD(sub_pose, Vec3 ( v.first[0], v.first[1], v.first[2]) );
        res += gaussian(heading, exp[0], covariance) * v.second;
      }
    );
    res /= (s::pow(2, 0.5) * covariance * s::pow(M_PI, 0.5));
    return res;
  };
  return s::make_pair(update, prior);
}
