import collections
import math
import random
from enum import Enum

import numpy as np

from scipy.cluster.hierarchy import fclusterdata

import shm

from auv_math.math_utils import rotate
from misc.hydro2trans import Localizer
from mission.constants.config import HYDROPHONES_PINGER_DEPTH
from mission.constants.region import PINGER_FREQUENCY, TRACK_MAG_THRESH, TRACK_COOLDOWN_SAMPLES
from mission.framework.combinators import Sequential
from mission.framework.helpers import get_sub_position, get_sub_quaternion, \
                                      ConsistencyCheck
from mission.framework.position import GoToPosition
from mission.framework.movement import Depth, Heading, VelocityX, VelocityY
from mission.framework.primitive import Zero, Log
from mission.framework.task import Task
from mission.framework.stateful_tasks import StatefulTask

from hydrocode.scripts.udp_set_gain import set_gain

from mission.constants.config import track as settings

from mission.missions.will_common import is_mainsub


from conf.vehicle import VEHICLE
is_mainsub = VEHICLE == 'odysseus'


# WILL ITS THIS ONE!
STOP_OVER_PINGER = True

BACKWARDS = is_mainsub

heading_offset = 180 if BACKWARDS else 0
x_dir = -1 if BACKWARDS else 1

def get_clusterable(data):
  return np.array(data).reshape((len(data), 1))

class ThrusterSilencer(Task):
  def __init__(self):
    super().__init__()
    self.silence_time = None

  def in_silence(self):
    return not shm.settings_control.enabled.get()

  def schedule_silence(self, silence_time, silence_length):
    self.cancel_silence()
    self.silence_time = silence_time
    self.silence_length = silence_length

  def cancel_silence(self):
    self.silence_time = None
    shm.settings_control.enabled.set(1)

  def on_run(self):
    # self.logi("Time: %0.3f, Silence Time: %r" % (self.this_run_time,
    #     self.silence_time))

    if self.silence_time is None:
      return

    if self.this_run_time > self.silence_time + self.silence_length:
      shm.settings_control.enabled.set(1)
      self.silence_time = None

    elif self.this_run_time > self.silence_time:
      shm.settings_control.enabled.set(0)

PingData = collections.namedtuple("PingData", ["phases", "heading",
                                  "elevation", "sub_pos", "sub_quat"])

PINGS_LISTEN = 10
MIN_CONSISTENT_PINGS = 3

PINGER_PERIOD = 1.0

MAX_FOLLOW_HEADING_DEVIATION = 10
SLOW_DOWN_DISTANCE = settings.slow_down_dist
MIN_DEVIATING_PING_ELEVATION = 65
MAX_FOLLOW_SPEED = settings.max_speed
MIN_FOLLOW_SPEED = settings.min_speed
MAX_ONTOP_OF_PINGER_ELEVATION = 15 # Try 15 in real life

# There are two states:

# LISTEN:
# Maintain position and listen for pings until reaching some decision about
# which direction to move.
#   -> FOLLOW when we have decided in which direction to move

# FOLLOW:
# Move in the direction the sub believes the pinger to be. There are two ways
# to choose a direction: either point in the heading suggested by the phases,
# or use the phases to localize the pinger in 3D space and then go to that
# location. This is controlled by the method argument.
#   -> LISTEN if we lose confidence in the pinger direction
#   -> FINISH when ontop_of_pinger returns true. For example, ontop_of_pinger
#      could check whether a mission element known to be located at the
#      pinger is visible

class TrackMethod(Enum):
  heading = 0
  position = 1

def ontop_of_pinger_elevation(ping_data):
  return (ping_data is not None and
      ping_data.elevation < MAX_ONTOP_OF_PINGER_ELEVATION)

Pinger = collections.namedtuple('Pinger', ['position', 'heading', 'elevation'])

# TODO: Termination condition based on pinger elevation
class FindPinger(StatefulTask):
  def __init__(self, method, ontop_of_pinger=ontop_of_pinger_elevation, *args, **kwargs):
    super().__init__(*args, **kwargs)

    self.track_method = method
    self.ontop_of_pinger = ontop_of_pinger

  def on_first_run(self):
    assert isinstance(self.track_method, TrackMethod)

    self.follow_task = None
    self.heading_to_pinger = None

    set_gain()

    shm.hydrophones_settings.track_frequency_target.set(PINGER_FREQUENCY)
    shm.hydrophones_settings.track_magnitude_threshold.set(TRACK_MAG_THRESH)
    shm.hydrophones_settings.track_cooldown_samples.set(TRACK_COOLDOWN_SAMPLES)

    shm.navigation_settings.position_controls.set(1)
    shm.navigation_settings.optimize.set(0)

    self.localizer = Localizer(PINGER_FREQUENCY)
    self.has_made_progress = False

    self.hydro_watcher = shm.watchers.watcher()
    self.hydro_watcher.watch(shm.hydrophones_results_track)

    self.time_since_last_ping = self.this_run_time

    self.pinger_positions = collections.deque(maxlen=7)

    self.silencer = ThrusterSilencer()
    self.pinger = None

    self.ping_deviating_checker = ConsistencyCheck(7, 10)

    ontop_success = None
    if STOP_OVER_PINGER:
        ontop_success = 2
    else:
        ontop_success = 6

    self.ontop_checker = ConsistencyCheck(ontop_success, 5)

    self.pings = []

  def generate_states(self):
      return "listen", {
          "listen": {
            "enter": self.enter_listen,
            "exit": self.exit_listen,
            "tick": self.listen
          },
          "follow": {
            "enter": self.enter_follow,
            "tick": self.follow
          }
      }

  # Check for new pings, if there is one return its heading and elevation and add it to the
  # pings list
  def update_pings(self):
    self.silencer()

    # TODO Better way to filter out bad pings when thrusters are running.
    # If the watcher has not changed, there is no new ping
    if not self.hydro_watcher.has_changed():
      # TODO Do something here if too much time has passed since last ping.
      return None
    # There is a new ping!

    self.time_since_last_ping = self.this_run_time

    # TODO Will this be long after the watcher fired?
    # Need to ensure that there is little delay.
    results = shm.hydrophones_results_track.get()
    kalman = shm.kalman.get()

    phases = (results.diff_phase_x, results.diff_phase_y)

    if not self.localizer.is_valid(phases[0], phases[1]):
      self.logi("Warning: Measured phases (%0.3f %0.3f) are not possible given "
                "physical parameters" % \
                (phases[0], phases[1]))

    head, elev = self.localizer.get_heading_elevation(*phases)
    abs_head = (head + shm.kalman.heading.get()) % 360
    self.logi("Ping: Phases: (%0.3f %0.3f), Relative Heading: %0.5f, Absolute "
              "Heading: %0.3f, Elevation: %0.1f" % \
              (phases[0], phases[1], head, abs_head, elev))

    in_silence = self.silencer.in_silence()

    # TODO Better way to detect ping period.
    this_ping_time = \
        results.daemon_start_time + results.tracked_ping_time
    self.silencer.schedule_silence(this_ping_time + PINGER_PERIOD - 0.2, 3.0)

    self.logi("This time: %0.3f, This ping time %0.3f, Diff: %0.3f" % (self.this_run_time,
        this_ping_time, self.this_run_time - this_ping_time))

    # If the thrusters are not in silence, then we may have heard thruster
    # noise and thought it was a ping; ignore it
    if not in_silence:
      self.logi("Heard ping but thrusters are not silenced, ignoring")
      return None

    # Record ping data
    sub_pos = get_sub_position(kalman)
    sub_quat = get_sub_quaternion(kalman)
    ping_data = PingData(phases, head, elev, sub_pos, sub_quat)
    self.pings.append(ping_data)

    return ping_data

  def enter_listen(self):
    self.logi("Stopping to listen for pings")
    kalman = shm.kalman.get()

    shm.navigation_settings.position_controls.set(1)

    desires = shm.navigation_desires.get()
    desires.north = kalman.north
    desires.east = kalman.east
    shm.navigation_desires.set(desires)

    # Clear previous pings
    self.pings = []

  def exit_listen(self):
    self.logi("Exiting listen state")
    shm.navigation_settings.position_controls.set(0)

  def listen(self):
    self.update_pings()

    # If we haven't even gotten MIN_CONSISTENT_PINGS in total, there won't be
    # enough consistent ones, no point in continuing
    if len(self.pings) < MIN_CONSISTENT_PINGS:
      return

    # We want to make sure pings are consistent before following them. To do
    # this, first cluster the pings by heading
    headings, elevations = zip(*[self.localizer.get_heading_elevation(*ping.phases) for ping in self.pings])
    data = get_clusterable(headings)
    clusters = fclusterdata(data, 8, criterion="distance")

    # The best cluster is the one with the most pings in it
    counted = collections.Counter(clusters)
    best_cluster, n_best = max(counted.items(), key=lambda item: item[1])

    # To follow a heading, we require that at least three pings are in the
    # cluster which contains it
    if n_best >= MIN_CONSISTENT_PINGS:
      # Compute average phase for best cluster
      good_pings = [self.pings[i] for i, cluster_num in enumerate(clusters) \
                    if cluster_num == best_cluster]

      x_phases = [ping.phases[0] for ping in good_pings]
      y_phases = [ping.phases[1] for ping in good_pings]

      avg_phase_x = np.mean(x_phases)
      avg_phase_y = np.mean(y_phases)

      self.logi("Average phases (%f, %f) for best cluster %s" % \
                (avg_phase_x, avg_phase_y, str(self.pings)))

      if self.track_method == TrackMethod.position:
        # Localize pinger to a position
        for ping in good_pings:
          self.localizer.add_observation(ping.phases, ping.sub_pos, ping.sub_quat)

        est_pinger_pos = self.localizer.compute_position()
        self.pinger_positions.append(est_pinger_pos)

        self.logi("Localized pinger to: %s" % str(est_pinger_pos))
        self.logi("All estimated positions: %s" % str(self.pinger_positions))

        self.follow_position = est_pinger_pos
      elif self.track_method == TrackMethod.heading:
        # Update the heading and elevation of the pinger
        pinger_head, elev = self.localizer.get_heading_elevation(avg_phase_x, avg_phase_y)
        sub_head = shm.kalman.heading.get()

        self.heading_to_pinger = heading=(pinger_head + sub_head) % 360
        self.follow_elevation = elev

      return "follow"

  def elevation_to_distance(self, elevation):
    return HYDROPHONES_PINGER_DEPTH * math.tan(math.radians(elevation))

  def enter_follow(self):
    self.logi("Following a heading of %0.3f" % self.heading_to_pinger)

    self.follow_change_heading = Heading()
    self.follow_inital_heading = Heading(self.heading_to_pinger + heading_offset)
    self.follow_vel_x = VelocityX()
    self.follow_vel_y = VelocityY()

    distance_to_pinger = self.elevation_to_distance(self.follow_elevation)
    self.follow_vel_x(x_dir * self.get_follow_speed(distance_to_pinger))
    self.follow_vel_y(0.0)

  # Proportional control to slow down near the pinger
  def get_follow_speed(self, distance):
    # Start slowing down at SLOW_DOWN_DISTANCE
    speed = distance / SLOW_DOWN_DISTANCE

    # Don't do nothing dumb
    if speed > MAX_FOLLOW_SPEED:
        speed = MAX_FOLLOW_SPEED
    if speed < MIN_FOLLOW_SPEED:
        speed = MIN_FOLLOW_SPEED

    return speed

  def follow_heading(self, new_ping):
    distance_to_pinger = self.elevation_to_distance(new_ping.elevation)
    speed = self.get_follow_speed(distance_to_pinger)

    # Check if the ping suggests the pinger is in a heading deviating from
    # the current follow heading
    new_ping_heading = (new_ping.heading + shm.kalman.heading.get()) % 360
    heading_deviation = abs(new_ping_heading - self.heading_to_pinger)

    # When we are close to the pinger, headings will vary more, allow for
    # deviation

    # Note, this is fairly broken, because it assumes we get good pings
    # while moving! Realistically, we should just move forward for a set time,
    # and then get new heading. BUT, since the consistency check will really
    # just wait for the requisite number of pings (since its a safe assumption
    # that we're just getting garbage), we can essentially turn this into
    # a makeshift timer by changing the window size on consistency check (:139)
    if new_ping.elevation > MIN_DEVIATING_PING_ELEVATION:
      deviating_ping = heading_deviation > MAX_FOLLOW_HEADING_DEVIATION

      if deviating_ping:
        self.logi("Ping heading deviated from follow heading by %0.3f, more "
                  "than maximum allowed" % heading_deviation)
      else:
        self.heading_to_pinger = new_ping_heading

      if self.ping_deviating_checker.check(deviating_ping):
        self.logi("Consistently getting deviating pings! Stopping to listen.")
        self.ping_deviating_checker.clear()

        return "listen"

      self.logi("Going straight for the pinger!")
      self.follow_vel_x(x_dir * speed)
      self.follow_vel_y(0.0)
      self.follow_change_heading(self.heading_to_pinger + heading_offset)
    else:
      velocity = rotate([speed, 0], new_ping.heading)
      self.logi("We are close! Translating to pinger: Velocity (%0.3f, "
                "%0.3f)" % (velocity[0], velocity[1]))
      self.follow_vel_x(velocity[0])
      self.follow_vel_y(velocity[1])

    return None

  def follow_position(self):
    return None

  def follow(self):
    # Turn to face the pinger
    if not self.follow_inital_heading.finished:
        self.follow_inital_heading()
        return

    new_ping = self.update_pings()

    if new_ping is not None:
      # Check whether we are on top of the pinger
      ontop_of_pinger = self.ontop_of_pinger(new_ping)
      if (ontop_of_pinger):
        self.logi("Ontop of pinger validator returned true! Waiting for "
                  "consitent positives to terminate following.")

      if self.ontop_checker.check(ontop_of_pinger):
        self.finish()

      if self.track_method == TrackMethod.heading:
        return self.follow_heading(new_ping)
      elif self.track_method == TrackMethod.position:
        return self.follow_position(new_ping)

class OptimizablePinger(Task):
  def desiredModules(self):
    return []

  def on_first_run(self):
    self.subtask = FindPinger(TrackMethod.heading)
    self.has_made_progress = False

  def on_run(self):
      self.subtask()
      if self.subtask.finished:
        self.finish()

      self.has_made_progress = self.subtask.has_made_progress

def Full(): return Sequential(
    Log('Changing depth before hydrophones'),
    Depth(settings.depth),
    OptimizablePinger(),
)

full = Full()
