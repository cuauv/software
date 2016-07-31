import collections

import numpy as np

import aslam
import shm

from auv_math.math_utils import rotate
from mission.constants.config import BINS_DROP_ALTITUDE, \
                                     BINS_CAM_TO_ARM_OFFSET, \
                                     BINS_PICKUP_ALTITUDE, \
                                     BINS_SEARCH_DEPTH

from mission.framework.actuators import FireActuator
from mission.framework.combinators import Sequential, MasterConcurrent, Concurrent
from mission.framework.helpers import get_downward_camera, \
                                      get_downward_camera_center, \
                                      ConsistencyCheck, get_sub_position
from mission.framework.movement import Depth, Heading, VelocityX, VelocityY, Roll, \
                                       RelativeToInitialDepth, RelativeToCurrentDepth
from mission.framework.position import MoveX, MoveY, PositionalControl
from mission.framework.primitive import Log, FunctionTask, Zero
from mission.framework.search import SpiralSearch
from mission.framework.targeting import DownwardTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.track import Tracker
from mission.missions.ozer_common import Timeout

FAST_RUN = False
#may simply drop markers into open bin if running out of time

# ^ Should be an optimal mission runner mode!

cover = shm.bin_cover
yellow1 = shm.bin_yellow_1
yellow2 = shm.bin_yellow_2

GIVEN_DISTANCE = 2 # meters

PERCENT_GIVEN_DISTANCE_SEARCH = .2 # percent of given distance to back up and check
SEARCH_ADVANCE_DISTANCE = 1 # meters to advance with each zigzag

class OptimizableBins(Task):
  def desiredModules(self):
    return [shm.vision_modules.Bins]

  def on_first_run(self, mode):
    self.has_made_progress = False
    self.subtask = \
      Sequential(
        MasterConcurrent(Sequential(Timer(4.0), IdentifyBins("cover")), aslam.SimpleTarget(aslam.world.bin_one, np.array([0., 0., -1.]))),
        BinsTask()
      )
  
  def on_run(self, mode):
    self.subtask()
    if self.subtask.finished:
      self.finish()

class BinsTask(Task):
    """Drops markers into target bin

    Current setup:
    Search for bins
    Center over target bin (assume covered bin initially)
    Try two times to remove lid
    If succeed, center over bin and drop markers in
        If fail twice, switch target bin to uncovered bin and drop markers

    Start: near bins (used pinger to locate), any position
    Finish: centered over target bin, both markers dropped
    """
    def on_first_run(self, *args, **kwargs):
        self.logi("Starting BinsTask task")
        self.init_time = self.this_run_time

        self.identify_bins = IdentifyBins("cover")

        # TODO Do a search for the bin after uncovering.
        self.tasks = Sequential(Timeout(Depth(BINS_SEARCH_DEPTH), 10),
                                Timeout(SearchBinsTask(), 45),
                                Timeout(self.identify_bins, 25),
                                FunctionTask(lambda: self.set_heading(shm.kalman.heading.get())),
                                UncoverBin(), DiveAndDropMarkers())

        # Below disabled until we get yellow cutout vision working.
        '''
                                MasterConcurrent(CheckBinsInSight(), Depth(BINS_SEARCH_DEPTH)),
                                IdentifyBins("not cover", heading=lambda: self.bins_heading,
                                             uncovered_bin_vector=lambda: self.identify_bins.uncovered_bin_vector),
                                PositionalControl(),
                                DiveAndDropMarkers())
        '''

    def set_heading(self, heading):
        self.bins_heading = heading

    def on_run(self):
        if self.tasks.finished:
            self.finish()

        self.tasks()

    def on_finish(self):
        self.logi("Bins completed!")
        self.logv('BinsTask task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))

class SearchBinsTask(Task):
    """Uses SpiralSearch in a MasterConcurrent with CheckBinsInSight"""
    def on_first_run(self, *args, **kwargs):
        self.logi("Looking for bins...")
        self.logv("Starting SearchBinsTask")
        self.init_time = self.this_run_time

        search_task = SpiralSearch(meters_per_revolution=1.0, optimize_heading=True, min_spin_radius=1000000)
        self.task = MasterConcurrent(CheckBinsInSight(), search_task)

    def on_run(self):
        self.task()
        if self.task.has_ever_finished:
            Zero()()
            self.finish()

    def on_finish(self):
        self.logi("Found bin!")
        self.logv('SearchBins task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))

class CheckBinsInSight(Task):
    """ Checks if the desired bin is in sight of the camera
    Used in SearchBinsTask as MasterConcurrent's end condition"""
    def on_first_run(self, *args, **kwargs):
        self.logv("Checking if bins in sight")
        self.init_time = self.this_run_time
        self.seen_checker1 = ConsistencyCheck(6, 8)
        self.seen_checker2 = ConsistencyCheck(6, 8)

    def on_run(self):
        cover_results = cover.get()
        yellow_results = yellow1.get()
        if self.seen_checker1.check(cover_results.probability > 0.0) or \
           self.seen_checker2.check(yellow_results.probability > 0.0):
            self.finish()

    def on_finish(self):
        self.logv('CheckBinsInSight task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))

TrackedBin = collections.namedtuple("TrackedBin", ["x", "y"])

class IdentifyBins(Task):
    """ Identifies which bin to drop markers into, centers over it """
    def on_first_run(self, run_type, heading=None, uncovered_bin_vector=None):
        self.logi("Centering over bins...")
        self.logv("Starting IdentifyBins task")

        self.center_valid = False
        self.center_coords = (0, 0)

        self.task = DownwardTarget(px=0.0025, py=0.0025,
                                   point=lambda: self.center_coords,
                                   valid=lambda: self.center_valid)

        self.center_checker = ConsistencyCheck(15, 17)
        self.align_checker = ConsistencyCheck(6, 6)
        # TODO start alignment task.
        self.init_time = self.this_run_time

        self.uncovered_bin_vector = None
        self.seen_two = False

        cam = get_downward_camera()
        self.cover_tracker = Tracker(cam['width'] * 0.15)
        self.yellow_tracker = Tracker(cam['width'] * 0.15)

    def on_run(self, run_type, heading=None, uncovered_bin_vector=None):
        yellows = [TrackedBin(b.center_x, b.center_y) if b.probability > 0.0 else None for b in [yellow1.get(), yellow2.get()] ]
        cover_g = cover.get()
        covert = TrackedBin(cover_g.center_x, cover_g.center_y) if cover_g.probability > 0.0 else None

        self.consistent_bins = self.yellow_tracker.track(*yellows)
        self.consistent_cover = self.cover_tracker.track(covert, None)

        def calculate_bin_vector(bin1, bin2):
          body_frame = [(bin1.x, bin1.y), (bin2.x, bin2.y)]
          world_frame = [np.array(rotate(body, -shm.kalman.heading.get())) for body in body_frame]
          bin2bin = world_frame[1] - world_frame[0]
          return bin2bin / np.linalg.norm(bin2bin)

        if any(self.consistent_cover) and any(self.consistent_bins):
          if run_type == "cover":
            good_cover = self.consistent_cover[0]
            if good_cover is None:
              good_cover = self.consistent_cover[1]
            good_yellow = self.consistent_bins[0]
            if good_yellow is None:
              good_yellow = self.consistent_bins[1]

            bin2cover_hat = calculate_bin_vector(good_yellow, good_cover)

            if self.uncovered_bin_vector is None:
              # TODO Take average here.
              self.uncovered_bin_vector = bin2cover_hat
              self.logi("Discovered cover to bin world vector %0.2f %0.2f" % \
                        (self.uncovered_bin_vector[0], self.uncovered_bin_vector[1]))

        if run_type == "cover":
          cands = self.consistent_cover + self.consistent_bins
        else:
          if all(self.consistent_bins) and uncovered_bin_vector is not None:
            bin2bin = calculate_bin_vector(self.consistent_bins[0], self.consistent_bins[1])
            if bin2bin.dot(uncovered_bin_vector()) > 0:
              index = 1
            else:
              index = 0

            if not self.seen_two:
              self.seen_two = True
              self.uncovered_ind = index
              self.logi("Chose bin with index %d: current coords %d %d" % \
                        (self.uncovered_ind, self.consistent_bins[self.uncovered_ind].x, self.consistent_bins[self.uncovered_ind].y))
            else:
              if self.uncovered_ind == index:
                self.logv("Confirmed uncovered bin has index %d" % index)
              else:
                self.logi("WARNING: Detected new uncovered bin index %d!" % index)

          if not self.seen_two:
            self.logv("Did not find two yellow!")
            cands = self.consistent_bins + self.consistent_cover
          else:
            cands = [self.consistent_bins[self.uncovered_ind], self.consistent_bins[1 - self.uncovered_ind]] + self.consistent_cover

        for i, cand in enumerate(cands):
          if cand is not None:
            self.logv("Found good bin of index %d" % i)
            self.center_valid = True
            self.center_coords = cand.x, cand.y
            break
        else:
          self.logv("No good bins found to center on!")
          self.center_valid = False

        # Assumes cover and contours from same camera
        target = get_downward_camera_center()
        # TODO Increase deadband as time increases.
        self.task(target=target, deadband=(25, 25))

        if self.center_checker.check(self.task.finished):
          if run_type == "cover" or heading is None:
            if heading is None:
              target_heading = shm.kalman.heading.get() + cover_g.angle
            else:
              target_heading = heading()

            align_task = Heading(target_heading, deadband=0.5)
            align_task()
            if self.align_checker.check(align_task.finished):
                VelocityX(0)()
                VelocityY(0)()
                self.finish()

          else:
            self.finish()
        else:
            self.align_checker.clear()

    def on_finish(self):
        self.logi("Centered!")
        self.logv('IdentifyBins task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))


class TwoTries(Task):
    """Keeps track of how many times sub tried to uncover bin, changes variable FAST_RUN to True if sub was unable to take off bin cover

    Note: tried to keep logic as generic as possible, with self.attempt_task and self.check_done so can be reused for other missions

    Start: centered over covered bin, no markers dropped
    Finish: centered over covered bin, either both or no markers dropped
    """
    def on_first_run(self, *args, **kwargs):
        self.logv("Starting TwoTries task")
        self.init_time = self.this_run_time

        self.attempt_task = UncoverBin()
        self.check_done = check_uncovered

        self.success = False
        self.tries_completed = 0

    def on_run(self):
        if self.tries_completed==0:
            if not self.attempt_task.has_ever_finished:
                self.attempt_task()
            else:
                if self.check_done():
                    self.success = True
                    self.finish()
                else:
                    self.tries_completed = 1
                    self.attempt_task = UncoverBin()
        else: #one completed try, one try left
            if not self.attempt_task.has_ever_finished:
                self.attempt_task()
            else:
                self.success = self.check_done()
                self.finish()

    def on_finish(self):
        self.logv('TwoTries task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))

def get_pool_depth(log, depth):
  alt = shm.dvl.savg_altitude.get()
  if alt < 0.7:
    log("ALTITUDE TOO LOW; USING HARDCODE")
    pool_depth = 4.7
  else:
    pool_depth = depth + alt

  return pool_depth

class UncoverBin(Task):
    """Uses thruster arm to remove bin cover.

    Start: centered over covered bin, no markers dropped
    Finish: centered over now-uncovered bin
    """
    def on_first_run(self, slide_direction="right", *args, **kwargs):
        self.logi("Starting UncoverBin task")
        self.init_time = self.this_run_time

        if slide_direction == "right":
          factor = 1
        elif slide_direction == "left":
          factor = -1
        else:
          raise Exception("Invalid slide direction %s for UncoverBin" % slide_direction)

        self.initial_depth = shm.kalman.depth.get()
        pool_depth = get_pool_depth(self.logw, self.initial_depth)

        self.set_depth = Timeout(Depth(pool_depth - BINS_PICKUP_ALTITUDE, deadband=0.01), 15)
        self.initial_position = get_sub_position()

        SLIDE_SPEED = 0.35
        self.pickup = MasterConcurrent(Sequential(Timer(0.7 * 6), Roll(0, error=90), Timer(0.3 * 2),
                                       Timed(Concurrent(RelativeToCurrentDepth(-2.0), Roll(-15 * factor, error=90)), 2.0 * 1),
                                       RelativeToCurrentDepth(0, error=2), Roll(-30 * factor, error=90)), VelocityY(SLIDE_SPEED * factor))
        self.drop = Sequential(Roll(90 * factor, error=20), Timer(1.0), Roll(120 * factor, error=40), Timer(0.5), Roll(90 * factor, error=20),
                               Timer(0.3), Roll(45 * factor), Timer(0.5), Roll(0))
        #self.return_to_bin = GoToPosition(lambda: self.initial_position[0], lambda: self.initial_position[1], depth=self.initial_depth)
        self.return_to_bin = Sequential(Timed(VelocityY(-1.0 * factor), 1.8), VelocityY(0.0),
                                        Timeout(MoveX(-BINS_CAM_TO_ARM_OFFSET, deadband=0.03), 10))
        self.tasks = Sequential(
                       Timeout(MoveY(-0.85 * factor, deadband=0.025), 10),
                       Timeout(MoveX(BINS_CAM_TO_ARM_OFFSET, deadband=0.015), 10),
                       self.set_depth,
                       Roll(60 * factor, error=20),
                       self.pickup,
                       VelocityY(0.0),
                       RelativeToInitialDepth(-0.4, error=2),
                       Timed(VelocityY(1.0 * factor), 1.0),
                       VelocityY(0.0),
                       self.drop,
                       Timed(VelocityY(-1.0 * factor), 1.0),
                       self.return_to_bin
                     )

    def on_run(self, *args):
        self.tasks()
        if self.tasks.finished:
            self.finish()

    def on_finish(self):
        self.logv('UncoverBin task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))

DropMarkersTogether = lambda: \
  Sequential(MoveY(-0.15, deadband=0.02), FireActuator("both_markers", 0.4),
  Log("Fired both droppers!"), MoveY(0.15, deadband=0.02))

DropMarkersSeperate = lambda: \
  Sequential(MoveY(-0.2), FireActuator("right_marker", 0.2),
  Log("Fired right dropper!"), MoveY(0.4),
  FireActuator("left_marker", 0.1), Log("Fired left dropper!"), MoveY(-0.2))

DropMarkers = DropMarkersTogether

class DiveAndDropMarkers(Task):
    """Drops markers into target bin

    Will need to lower self towards bin for better dropping accuracy

    Start: centered over target bin, no markers dropped
    Finish: centered over target bin, both markers dropped
    """
    def on_first_run(self, *args, **kwargs):
        self.logi("Starting DropMarkers task")
        self.init_time = self.this_run_time

        self.initial_depth = shm.kalman.depth.get()
        pool_depth = get_pool_depth(self.logw, self.initial_depth)
        self.set_depth = Depth(pool_depth - BINS_DROP_ALTITUDE, deadband=0.01)
        self.return_depth = Depth(self.initial_depth, deadband=0.01)

        self.timer = Timer(2.0)
        # TODO More accurate marker to bin alignment (Use the marker's position on the sub).
        self.seq = Sequential(self.set_depth, DropMarkers(), self.return_depth)

        self.timer()

    def on_run(self):
        self.seq()
        if self.seq.has_ever_finished:
            self.finish()

    def on_finish(self):
        self.logi("Dropped markers!")
        self.logv('DiveAndDropMarkers task finished in {} seconds!'.format(
            self.this_run_time - self.init_time))
