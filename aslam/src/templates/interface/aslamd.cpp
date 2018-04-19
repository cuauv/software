#include <mutex>
#include <thread>
#include <chrono>
#include <iostream>

#include <conf/map.hpp>
#include <conf/vehicle.hpp>
#include <libshm/c/shm.h>
#include <auvlog/client.h>

#include "../math/common.hpp"
#include "../math/fastslam.cpp"
#include "../math/observations.hpp"
#include "../net/server.hpp"

struct aslam_sub      shm_aslam_sub;
struct aslam_settings shm_aslam_settings;
struct kalman         shm_kalman;

#define DEG_TO_RAD(val) val * M_PI / 180
  
$!sub['state_type']!$ readKalman(kalman shm_kalman) {
  Vec6 vec;
  vec << shm_kalman.north,
         shm_kalman.east,
         shm_kalman.depth,
         DEG_TO_RAD(shm_kalman.heading),
         DEG_TO_RAD(shm_kalman.pitch),
         DEG_TO_RAD(shm_kalman.roll);
  return vec;
}

int main(int argc, char* argv[]) {
  shm_init();
  auvlog_init();

  conf::map m = conf::load_map();

  AUVLOG("Loaded locale configuration file");

  conf::vehicle v = conf::load_vehicle();

  s::map<s::string, conf::camera> cameras;
  for (unsigned int i = 0; i < v.cameras.size(); i++) {
    conf::camera c = v.cameras[i];
    AUVLOG("Initialized camera \"" + c.tag + "\"");
    cameras[c.tag] = c;
  };

  AUVLOG("Loaded " + s::to_string(v.cameras.size()) + " cameras from vehicle configuration file");

  shm_getg(kalman, shm_kalman);

  Map map ( readKalman(shm_kalman) );

  AUVLOG("Initialized map with sub position set to current Kalman position");

  s::map<s::string, conf::object> objects;
  s::for_each(
    m.objects.begin(),
    m.objects.end(),
    [&](const conf::object& o) { objects[o.name] = o; }
  );

<!--(for o in objects)-->
  {
    auto iter = objects.find("$!o['name']!$");
    if (iter == objects.end()) throw s::runtime_error("Object $!o['name']!$ was specified in aslam.conf but not found in locale configuration file!");
    conf::object o = iter->second;
    s::for_each(
      map.particles.begin(),
      map.particles.end(),
      [&](auto& p) { 
        $!o['state_type']!$ pos = $!o['state_type']!$::Zero();
        $!o['covariance_type']!$ cov = $!o['covariance_type']!$::Zero();
  <!--(for d in range(0, o['dimensionality']))-->
        pos[$!d!$] = o.initial_position[$!d!$];
        cov($!d!$, $!d!$) = o.initial_covariance($!d!$, $!d!$);
  <!--(end)--> 
        p.first.$!o['name']!$ = PointCloud<$!o['state_type']!$, $!o['covariance_type']!$> ( pos, cov );
      }
    ); 
    AUVLOG("Successfully initialized object: " + o.name + " bound to SHM group \"aslam_" + o.name + "\"")
  }
<!--(end)-->

  auto _iter = objects.find("pipe_to_wire");
  if (_iter == objects.end()) throw s::runtime_error("Pipe to wire not found in locale configuration file!");
  conf::object o = _iter->second;
  wire_pipe_results shm_wire_pipe_results;
  shm_wire_pipe_results.seen = false;
  shm_wire_pipe_results.north   = o.initial_position[0];
  shm_wire_pipe_results.east    = o.initial_position[1];
  shm_wire_pipe_results.depth   = o.initial_position[2];
  shm_wire_pipe_results.heading = o.initial_position[3]; 
  shm_setg(wire_pipe_results, shm_wire_pipe_results);
  AUVLOG("Swireal wire guesswirecale position")

  _iter = objects.find("pipe_to_buoys");
  if (_iter == objects.end()) throw s::runtime_error("Pipe to wire not found in locale configuration file!");
  o = _iter->second;
  buoys_pipe_results shm_buoys_pipe_results;
  shm_buoys_pipe_results.seen = false;
  shm_buoys_pipe_results.north   = o.initial_position[0];
  shm_buoys_pipe_results.east    = o.initial_position[1];
  shm_buoys_pipe_results.depth   = o.initial_position[2];
  shm_buoys_pipe_results.heading = o.initial_position[3]; 
  shm_setg(buoys_pipe_results, shm_buoys_pipe_results);
  AUVLOG("Set initial pipe to buoys guess from locale position")

  s::chrono::time_point<std::chrono::high_resolution_clock> prev, curr;
  s::chrono::duration<double> dur;
  double dt;
  
  prev = s::chrono::high_resolution_clock::now();

  Vec6 prev_state = readKalman(shm_kalman);
  int iter = 0;

  s::mutex obs_vector_mutex;
  s::vector<s::function<void(Map&)>> observations;

<!--(for s in observables)-->
  {
    const auto inner = [&]() {
      watcher_t w = create_watcher();
      shm_watch($!s['group']!$, w);
      $!s['group']!$ results;
      for(;;) {
        wait_watcher(w, false);
        shm_getg($!s['group']!$, results);
        auto res = $!s['function']!$(results, cameras);
        auto update = res.first;
        auto prior  = res.second;
  <!--(if s['type'] == 'singular')-->
        double likelihood = prior(map.combined_sub, map.combined_$!s['object']!$) * s::max(1.0, s::pow(map.combined_$!s['object']!$.covariance().norm(), 3));
        if (likelihood < 0) continue;
        if (s::isnan(likelihood)) {
          AUVLOG("Rejected observation of $!s['object']!$ with reason: prior was NaN!")
          continue;
        }
        if (likelihood < $!s['rejection_threshold']!$) {
          AUVLOG("Rejected observation of $!s['object']!$ with reason: prior " + s::to_string(likelihood) + " was less than threshold $!s['rejection_threshold']!$")
          continue;
        }
        {
          s::lock_guard<s::mutex> lock (obs_vector_mutex);
          observations.push_back([=](Map& m) { m.apply_$!s['object']!$(update, prior); });
        }
        AUVLOG("Applied observation of $!s['object']!$ with prior " + s::to_string(likelihood))
  <!--(elif s['type'] == 'disjoint')-->
    <!--(for o in s['objects'])-->
        double likelihood_$!o!$ = prior(map.combined_sub, map.combined_$!o!$) * s::max(1.0, s::pow(map.combined_$!o!$.covariance().norm(), 3));
        if (s::isnan(likelihood_$!o!$)) {
          AUVLOG("Rejected observation of $!o!$ with reason: prior was NaN!")
          continue;
        }
    <!--(end)-->
        s::vector<double> likelihoods { $!', '.join(['likelihood_' + o for o in s['objects']])!$ };
        double max = *s::max_element(likelihoods.begin(), likelihoods.end());
    <!--(for o in s['objects'])-->
        if (likelihood_$!o!$ == max) {
          if (max < $!s['rejection_threshold']!$) {
            AUVLOG("Rejected observation of $!o!$ with reason: prior " + s::to_string(max) + " was less than threshold $!s['rejection_threshold']!$")
            continue;
          }
          {
            s::lock_guard<s::mutex> lock (obs_vector_mutex);
            observations.push_back([=](Map& m) { m.apply_$!o!$(update, prior); });
          }
          AUVLOG("Applied observation resolved to $!o!$ with prior " + s::to_string(likelihood_$!o!$))
        }
    <!--(end)-->
  <!--(end)-->
      };
    };
    auto thread = s::thread(inner);
    thread.detach();
  <!--(if s['type'] == 'singular')-->
    AUVLOG("Launched observation thread of $!s['object']!$ synced with SHM group $!s['group']!$")
  <!--(elif s['type'] == 'disjoint')-->
    AUVLOG("Launched observation thread of $!', '.join(s['objects'])!$ synced with SHM group $!s['group']!$")
  <!--(end)-->
  }
<!--(end)-->

  s::thread server_thread (spawn_server, s::ref(map));
  server_thread.detach();
  AUVLOG("Launched server listening on port 8888")
 
  for (;;) {
    iter++;

    curr = s::chrono::high_resolution_clock::now();
    dur  = curr - prev;
    dt   = (double) dur.count();
    prev = curr;
    shm_getg(kalman, shm_kalman);
    shm_getg(aslam_settings, shm_aslam_settings);

    Mat6 control_covariance;
    control_covariance(0, 0) = shm_aslam_settings.control_covariance_x;
    control_covariance(1, 1) = shm_aslam_settings.control_covariance_y;
    control_covariance(2, 2) = shm_aslam_settings.control_covariance_d;
    control_covariance(3, 3) = shm_aslam_settings.control_covariance_h;
    control_covariance(4, 4) = shm_aslam_settings.control_covariance_p;
    control_covariance(5, 5) = shm_aslam_settings.control_covariance_r;
  
    Vec6 state = readKalman(shm_kalman);
    Vec6 delta = state - prev_state;
    prev_state = state;

    if (shm_aslam_settings.reset) {
      s::for_each(
        map.particles.begin(),
        map.particles.end(),
        [&](auto& p) {
          p.first.pose = state;
        }
      );
      shm_aslam_settings.reset = false;
      shm_setg(aslam_settings, shm_aslam_settings);
    } else {
      s::for_each(
        map.particles.begin(),
        map.particles.end(),
        [&](auto& p) {
          Vec6 del = p.first.pose + delta;
          Mat6 cov = control_covariance * dt;
          p.first.pose = multivariate_gauss(del, cov);
          // Copy over depth, heading, pitch, and roll. Allow N/E to accumulate residual error.
          // for (unsigned int ind = 2; ind < 6; ind++) p.first.pose[ind] = state[ind];
          for (unsigned int ind = 0; ind < 6; ind++) p.first.pose[ind] = state[ind];
      }
      );
    };
  
    // Cannot apply observations first-round because priors will be garbage!
    bool sleep = false;
    {
      s::lock_guard<s::mutex> lock(obs_vector_mutex);
      if (observations.size() == 0) {
        sleep = true;
      };
      if (iter > 1) {
        s::for_each(
          observations.begin(),
          observations.end(),
          [&](const s::function<void(Map&)> func) {
            func(map);
          }
        );
        observations.clear();
      }
    }

    map.step_resample();

    map.estimate();
  
<!--(for o in objects)-->
    {
      map.combined_$!o['name']!$.normalize();
      $!o['state_type']!$ mean = map.combined_$!o['name']!$.mean();
      // $!o['covariance_type']!$ covariance = map.combined_$!o['name']!$.covariance();
      $!o['covariance_type']!$ covariance = map.combined_intrinsic_covariance_$!o['name']!$;
      $!o['shm_group']!$ shm_$!o['shm_group']!$;
  <!--(for ind in range(0, o['dimensionality']))-->
      shm_$!o['shm_group']!$.$!dimensions[ind]!$ = mean[$!ind!$];
      shm_$!o['shm_group']!$.$!dimensions[ind]!$_uncertainty = covariance($!ind!$, $!ind!$);
  <!--(end)-->
      shm_setg($!o['shm_group']!$, shm_$!o['shm_group']!$);
    }
<!--(end)-->

    shm_aslam_sub.north   = map.combined_sub[0];
    shm_aslam_sub.east    = map.combined_sub[1];
    shm_aslam_sub.depth   = map.combined_sub[2];
    shm_aslam_sub.heading = map.combined_sub[3];
    shm_aslam_sub.pitch   = map.combined_sub[4];
    shm_aslam_sub.roll    = map.combined_sub[5];

    shm_setg(aslam_sub, shm_aslam_sub);
 
    if (sleep) s::this_thread::sleep_for(s::chrono::milliseconds(10));
   
  };
};
