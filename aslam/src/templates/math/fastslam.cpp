#include "fastslam.h"

Particle::Particle(const $!sub['state_type']!$& initial_pose) {
  this->pose   = initial_pose;
};

Particle::Particle(const Particle& other) {
  this->pose    = Vec6 ( other.pose );
<!--(for o in objects)-->
  this->$!o['name']!$ = other.$!o['name']!$;
<!--(end)-->
};

Particle::~Particle () {
};

Map::Map(const $!sub['state_type']!$& initial_pose) {
  for (int i = 0; i < NUM_COMBINED_STATE_PARTICLES; i++) {
    this->particles.push_back(s::make_pair(Particle(initial_pose), 1.0 / NUM_COMBINED_STATE_PARTICLES));
  };
};

void Map::step_controls(const $!sub['state_type']!$& rate, const $!sub['covariance_type']!$& _covariance, double dt) {
  $!sub['state_type']!$ delta           = dt * rate;
  $!sub['covariance_type']!$ covariance = dt * _covariance;
  s::for_each(
    this->particles.begin(),
    this->particles.end(),
    [&](auto& p) {
      $!sub['state_type']!$ prev = p.first.pose;
      $!sub['state_type']!$ real_delta = multivariate_gauss(delta, covariance);
      double heading = prev[3] + real_delta[3];
      double pitch   = prev[4] + real_delta[4];
      double roll    = prev[5] + real_delta[5];
      p.first.pose <<  prev[0] + real_delta[0] * cos(heading),
                       prev[1] + real_delta[1] * sin(heading),
                       prev[2] + real_delta[2],
                       heading,
                       pitch,
                       roll;
    }
  );
};

void Map::step_resample() {
  double sum = s::accumulate(
    this->particles.begin(),
    this->particles.end(),
    0.0,
    [](auto x, auto y) { return x + y.second; }
  );
  s::for_each(
    this->particles.begin(),
    this->particles.end(),
    [&](auto& p) {
      p.second /= sum;
    }
  );
  {
    this->particle_mutex.lock();
    this->particles = frequency_resample_transform(this->particles, [](const Particle& val) { return val; });
    this->particle_mutex.unlock();
  }
};

<!--(for o in objects)-->
template<typename A, typename B>
void Map::apply_$!o['name']!$(A update, B prior) {
  s::for_each(
    this->particles.begin(),  
    this->particles.end(),
    [&](auto& p) {
      double pri = prior(p.first.pose, p.first.$!o['name']!$);
      p.first.$!o['name']!$.update([&](const $!o['state_type']!$& pos) {
        double res = update(p.first.pose, pos); 
        return (s::isfinite(res) && res > 0) ? res : INVALID_RATIO_REPLACEMENT_VALUE;
      });
      p.second *= 1 + s::isfinite(pri) ? pri : 0.0;
    }
  );
};

<!--(end)-->

void Map::estimate() {
  double mul = 1.0 / static_cast<double>(this->particles.size());
  this->combined_sub = $!sub['state_type']!$::Zero();
<!--(for o in objects)-->
  this->combined_$!o['name']!$ = PointCloud<$!o['state_type']!$, $!o['covariance_type']!$>();
  this->combined_$!o['name']!$.particles.clear();
  this->combined_intrinsic_covariance_$!o['name']!$ = $!o['covariance_type']!$::Zero();
<!--(end)-->
  s::vector<double> headings;
  s::vector<double> pitches;
  s::vector<double> rolls;
  s::for_each(
    this->particles.begin(),
    this->particles.end(),
    [&](const auto& p) {
      this->combined_sub[0] += p.first.pose[0] * mul;
      this->combined_sub[1] += p.first.pose[1] * mul;
      this->combined_sub[2] += p.first.pose[2] * mul;
      headings.push_back(p.first.pose[3]);
      pitches.push_back(p.first.pose[4]);
      rolls.push_back(p.first.pose[5]);
<!--(for o in objects)-->
      this->combined_intrinsic_covariance_$!o['name']!$ += p.first.$!o['name']!$.intrinsic_covariance * mul;
      this->combined_$!o['name']!$.particles.insert(
        this->combined_$!o['name']!$.particles.end(),
        p.first.$!o['name']!$.particles.begin(),
        p.first.$!o['name']!$.particles.end()
      );
<!--(end)-->
    }
  );

  this->combined_sub[3] = average_circle(headings);
  this->combined_sub[4] = average_circle(pitches);
  this->combined_sub[5] = average_circle(rolls);
};
