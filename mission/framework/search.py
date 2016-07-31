import numpy as np
import math
import aslam

import shm

from auv_math.math_utils import rotate
from auv_math.quat import Quaternion, quat_from_axis_angle
from auv_python_helpers.angles import abs_heading_sub_degrees
from mission.framework.combinators import Sequential
from mission.framework.helpers import call_if_function, ConsistencyCheck
from mission.framework.movement import Heading, Pitch, Roll, \
                                       RelativeToInitialHeading, VelocityX, \
                                       RelativeToCurrentHeading
from mission.framework.position import MoveXRough, MoveYRough, GoToPosition
from mission.framework.task import Task
from mission.framework.timing import Timer
from auv_python_helpers.angles import heading_sub_degrees

class VelocitySwaySearch(Task):
    def make_repeat(self, forward, stride):
      self.repeat = Sequential(Timed(VelocityY(-1.0), stride),
                               Timed(VelocityY(1.0), stride),
                               Timed(VelocityX(1.0), forward),
                               VelocityX(0.0))
     
    def on_first_run(self):
      self.make_repeat()

    def on_run(self):
      self.repeat()
      if self.repeat.finished:
        self.make_repeat()  

class SearchFor(Task):
    def on_first_run(self, search_task, visible, consistent_frames=(3, 5)):
        self.found_checker = ConsistencyCheck(*consistent_frames)

    def on_run(self, search_task, visible, consistent_frames=(3, 5)):
        visible = call_if_function(visible)
        if self.found_checker.check(visible):
            self.finish()
        else:
            search_task()

class PitchSearch(Task):
    def on_first_run(self, angle, dps=45):
        self.degrees_per_second = dps
        self.revolve_angle = 0
        self.revolve_task = RelativeToInitialHeading(current=shm.kalman.heading.get())
        Pitch(angle)()

    def on_run(self, *args):
        if self.last_run_time is None:
            return

        dt = self.this_run_time - self.last_run_time

        self.revolve_angle += self.degrees_per_second * dt
        self.revolve_task(self.revolve_angle)

        if self.revolve_angle > 360:
            self.revolve_task(360)
            Pitch(0)()
            self.finish()

class OrientationSearch(Task):
    """
       Revolves around the depth axis at an angle in degrees.
    """

    def on_first_run(self, angle, dps=45):
        self.conic_angle_rad = math.radians(angle)
        self.degrees_per_second = dps
        self.revolve_angle = 0
        self.initial_heading = shm.kalman.heading.get()

    def on_run(self, *args):
        if self.last_run_time is None:
            return

        dt = self.this_run_time - self.last_run_time

        self.revolve_angle += self.degrees_per_second * dt

        if self.revolve_angle > 390:
            Heading(self.initial_heading)()
            Pitch(0)()
            Roll(0)()
            self.finish()
            return

        rot_2d = rotate((0, 1), self.revolve_angle)
        rotation_axis = np.array((rot_2d[0], rot_2d[1], 0))

        conic_quat = quat_from_axis_angle(rotation_axis, self.conic_angle_rad)
        pointing_vector = conic_quat * np.array((0, 0, 1))

        # TODO How can we achieve the same effect without using heading?
        heading, pitch, roll = conic_quat.hpr()

        Heading(heading)()
        Pitch(pitch)()
        Roll(roll)()

class SwaySearch(Task):
    def maybe_add_roll(self, original, roll_direction):
        tasks = [original]
        if self.roll_extension:
            roll = -roll_direction * 45
            tasks.extend([Roll(roll, error=10), Timer(1.0), Roll(0, error=3)])
        return Sequential(*tasks)

    def on_first_run(self, width, stride, roll_extension=False):
        self.width = width
        self.stride = stride
        self.roll_extension = roll_extension

        self.repeat = Sequential(self.maybe_add_roll(MoveYRough(self.width / 2), 1),
                                 self.maybe_add_roll(MoveYRough(-self.width), -1),
                                 self.maybe_add_roll(MoveXRough(self.stride), -1))

    def make_repeat(self):
        self.repeat = Sequential(self.maybe_add_roll(MoveYRough(self.width), 1),
                                 self.maybe_add_roll(MoveXRough(self.stride), 1),
                                 self.maybe_add_roll(MoveYRough(-self.width), -1),
                                 self.maybe_add_roll(MoveXRough(self.stride), -1))

    def on_run(self, *args):
        self.repeat()

        if self.repeat.finished:
            self.make_repeat()

class SpiralSearch(Task):
  def calc_radius(self, theta, meters_per_revolution):
    return meters_per_revolution * theta / 360.

  def calc_position(self, theta, meters_per_revolution, relative_depth_range):
    radius = self.calc_radius(theta, meters_per_revolution)
    vector = np.array([math.cos(math.radians(theta)), math.sin(math.radians(theta)), 0])
    delta = radius * vector
    delta[2] = relative_depth_range * math.sin(math.radians(theta))
    return self.start_position + delta

  def on_first_run(self, *args, **kwargs):
    self.theta = 0
    self.start_position = aslam.sub.position()
    self.started_heading = False

  def on_run(self, meters_per_revolution = 1.3, deadband = 0.2, spin_ratio = 5, relative_depth_range = 0.0, heading_change_scale = None, optimize_heading = False, min_spin_radius=None):
    radius = self.calc_radius(self.theta, meters_per_revolution)
    target = self.calc_position(self.theta, meters_per_revolution, relative_depth_range)
    delta  = target - aslam.sub.position()
    # fake_target = target + (delta * 10)
    fake_target = target
    GoToPosition(fake_target[0], fake_target[1], depth = fake_target[2])()
    desired_heading = (spin_ratio * self.theta)
    if heading_change_scale is not None:
      desired_heading *= min(1.0, radius) * heading_change_scale
    if optimize_heading:
      desired_heading = math.degrees(math.atan2(delta[1], delta[0]))
      if min_spin_radius is not None:
        if (not abs_heading_sub_degrees(shm.kalman.heading.get(), desired_heading) < 10 or not radius > min_spin_radius) and not self.started_heading:
          desired_heading = shm.navigation_desires.heading.get()
        else:
          self.started_heading = True

    Heading(desired_heading % 360)()
    if np.linalg.norm(aslam.sub.position() - target) < deadband:
      self.theta += 10

class CheckSpinCompletion(Task):
    def on_first_run(self, n):
        self.heading = shm.kalman.heading.get()
        self.cumul = 0

    def on_run(self, n):
        new_heading = shm.kalman.heading.get()
        self.cumul += heading_sub_degrees(new_heading, self.heading)
        self.heading = new_heading

        if abs(self.cumul) > 360 * n:
            self.finish()

class HeadingSearch(Task):
    """
    Search by moving forward and oscillating heading
    """
    class Scan(Task):
        def on_run(self, target, *args, **kwargs):
            current = shm.kalman.heading.get()
            diff = heading_sub_degrees(target, current)
            inc = [-1, 1][diff > 0] * 12
            RelativeToCurrentHeading(inc)()
            if abs(diff) < 5:
                self.finish()

    class MoveIncrement(Task):
        def on_run(self, north, east, *args, **kwargs):
            GoToPosition(north, east)()
            current = aslam.sub.position()[:2]
            target = np.array([north, east])
            if sum((current - target) ** 2) < 0.25:
                self.finish()

    def on_first_run(self, initial_heading, heading_amplitude=45, stride=2.5, *args, **kwargs):
        self.initial_heading = initial_heading
        self.heading_amplitude = heading_amplitude
        self.stride = stride
        self.initial_pos = np.array(aslam.sub.position()[:2])
        self.new_pos = self.initial_pos
        heading_radians = math.radians(initial_heading)
        self.direction_vector = np.array([
            math.cos(heading_radians),
            math.sin(heading_radians),
        ])
        self.offset_len = 0
        self.new_task()

    def new_task(self):
        self.offset_len += self.stride;
        new_pos = self.initial_pos + self.direction_vector * self.offset_len
        self.task = Sequential(
            self.Scan(self.initial_heading + self.heading_amplitude),
            Timer(1.0),
            self.Scan(self.initial_heading - self.heading_amplitude),
            Timer(1.0),
            Heading(self.initial_heading),
            self.MoveIncrement(new_pos[0], new_pos[1]),
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.new_task()
