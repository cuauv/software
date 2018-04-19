#include "../interface/observable.h"
#include "../math/observations.hpp"

// This file is DEPRECATED, use aslam.conf instead

/*

  How To Add An Object:
  
  1. Add your object to SHM in a group labeled "aslam_{object name}".
  2. Add that SHM group to the "repeat_out" macro below.
  3. Add your object to the locale configuration file(s) - e.g. `teagle.conf`.

  How To Add An Observable:

  1. Make a function (or use a generic one) in math/observations.hpp that takes your SHM group and returns update and prior functions.
  2. Declare that function, templated on the SHM group, as "auto MyObjectObs = {the function}" below.
  3. Add a line to "ASLAM_MAKE_OBSERVABLES" below that calls ASLAM_MAKE_OBSERVABLE with:
      a. The name of the object as it appears in the locale configuration file.
      b. The SHM group.
      c. The function alias you just made.
      d. The prior rejection threshold ASLAM should use.

*/

auto RedBuoyObs = observationFromGenericForwardVisionArea<red_buoy_results, s::ratio<1, 10>>;
auto GreenBuoyObs = observationFromGenericForwardVisionArea<green_buoy_results, s::ratio<1, 10>>;
auto YellowBuoyObs = observationFromGenericForwardVisionArea<yellow_buoy_results, s::ratio<1, 10>>;
auto WireObs = observationFromWireGroup<wire_results, s::ratio<1, 6>>;
auto TorpedoesObs = observationFromTorpedoesGroup<torpedoes_results, s::ratio<3, 2>>;
// auto RecoveryAreaObs = observationFromRecoveryGroupArea<recovery_results, s::ratio<3, 2>>;

#define ASLAM_MAKE_OBSERVABLES()\
  ASLAM_MAKE_OBSERVABLE("red_buoy", red_buoy_results, RedBuoyObs, 0.0002);\
  ASLAM_MAKE_OBSERVABLE("green_buoy", green_buoy_results, GreenBuoyObs, 0.02);\
  ASLAM_MAKE_OBSERVABLE("yellow_buoy", yellow_buoy_results, YellowBuoyObs, 0.02);\
  ASLAM_MAKE_OBSERVABLE("wire", wire_results, WireObs, 0.02);\
  ASLAM_MAKE_OBSERVABLE("torpedoes", torpedoes_results, TorpedoesObs, 0.02);


//  ASLAM_MAKE_OBSERVABLE("recovery_area", recovery_results, RecoveryAreaObs, 0.002);

#define repeat_out(macro) \
  macro(aslam_pipe_to_buoys);\
  macro(aslam_red_buoy);\
  macro(aslam_green_buoy);\
  macro(aslam_yellow_buoy);\
  macro(aslam_pipe_to_wire);\
  macro(aslam_wire);\
  macro(aslam_bin_one);\
  macro(aslam_bin_two);\
  macro(aslam_torpedoes);\
  macro(aslam_recovery_tower);\
  macro(aslam_recovery_area);
