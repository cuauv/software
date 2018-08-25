import numpy as np
import math

import shm

from auv_math.math_utils import rotate
from auv_math.quat import quat_from_axis_angle
from auv_python_helpers.angles import abs_heading_sub_degrees
from mission.framework.combinators import Sequential, While
from mission.framework.helpers import call_if_function, ConsistencyCheck
from mission.framework.movement import (
    Heading, Pitch, Roll,
    RelativeToInitialHeading, VelocityX,
    RelativeToCurrentHeading, VelocityY
)
from mission.framework.position import MoveXRough, MoveYRough, GoToPosition
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.primitive import Zero
from mission.missions.ozer_common import ConsistentTask
from auv_python_helpers.angles import heading_sub_degrees

def _sub_position():
    return np.array([
        shm.kalman.north.get(),
        shm.kalman.east.get(),
        shm.kalman.depth.get(),
    ])

class VelocitySwaySearch(Task):
    def make_repeat(self, forward, stride, speed, rightFirst, checkBehind):
        dir = 1 if rightFirst else -1
        if checkBehind:
            self.repeat = Sequential(
                                Timed(VelocityX(-speed), forward),
                                Timed(VelocityX(speed), forward),
                                VelocityX(0),
                                Timed(VelocityY(speed * dir), stride),
                                VelocityY(0.0),
                                Timed(VelocityX(speed), forward),
                                VelocityX(0.0),
                                Timed(VelocityY(-speed * dir), stride),
                                VelocityY(0.0),
                                Timed(VelocityY(-speed * dir),stride),
                                VelocityY(0.0),
                                Timed(VelocityX(speed), forward),
                                VelocityX(0.0),
                                Timed(VelocityY(speed * dir), stride),
                                VelocityY(0.0))
        else:
            self.repeat = Sequential(
                                Timed(VelocityY(speed * dir), stride),
                                VelocityY(0.0),
                                Timed(VelocityX(speed), forward),
                                VelocityX(0.0),
                                Timed(VelocityY(-speed * dir), stride),
                                VelocityY(0.0),
                                Timed(VelocityY(-speed * dir),stride),
                                VelocityY(0.0),
                                Timed(VelocityX(speed), forward),
                                VelocityX(0.0),
                                Timed(VelocityY(speed * dir), stride),
                                VelocityY(0.0))

    def on_first_run(self, forward = 1, stride=1, speed=0.3, rightFirst=True, checkBehind=False):
        self.make_repeat(forward, stride, speed, rightFirst, checkBehind)

    def on_run(self, forward = 1, stride=1, speed=0.3, rightFirst=True, checkBehind=False):
        self.repeat()
        if self.repeat.finished:
            self.make_repeat(forward, stride, speed, rightFirst, checkBehind)

class VelocityTSearch(Task):
    def make_repeat(self, forward, stride, rightFirst, checkBehind):
        dir = 1 if rightFirst else -1
        self.back_check = Timed(VelocityX(-.3), forward)
        self.checked_b = not checkBehind
        self.repeat = Sequential(
                                Timed(VelocityX(.3 ), forward),
                                VelocityX(0.0),
                                Timed(VelocityY(.3 * dir), stride),
                                VelocityY(0.0),
                                Timed(VelocityY(-.3 * dir), stride * 2),
                                VelocityY(0.0),
                                Timed(VelocityY(.3 * dir),stride),
                                VelocityY(0.0),
                                )

    def on_first_run(self, forward = 1, stride=1, rightFirst=True, checkBehind=False):
        self.make_repeat(forward, stride, rightFirst, checkBehind)

    def on_run(self, forward = 1, stride=1, rightFirst=True, checkBehind=False):
        if not self.checked_b:
            self.back_check()
        else:
            self.repeat()
        if self.back_check.finished:
            self.checked_b=True
        if self.repeat.finished:
            self.make_repeat(forward, stride, rightFirst, checkBehind)

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
    self.start_position = _sub_position()
    self.started_heading = False

  def on_run(self, meters_per_revolution = 1.3, deadband = 0.2, spin_ratio = 1, relative_depth_range = 0.0, heading_change_scale = None, optimize_heading = False, min_spin_radius=None):
    radius = self.calc_radius(self.theta, meters_per_revolution)
    target = self.calc_position(self.theta, meters_per_revolution, relative_depth_range)
    delta  = target - _sub_position()
    # fake_target = target + (delta * 10)
    fake_target = target
    GoToPosition(fake_target[0], fake_target[1], depth = fake_target[2])()
    desired_heading = (spin_ratio * self.theta) + 90
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
    if np.linalg.norm(_sub_position() - target) < deadband:
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
            current = _sub_position()[:2]
            target = np.array([north, east])
            if sum((current - target) ** 2) < 0.25:
                self.finish()

    def on_first_run(self, initial_heading, heading_amplitude=45, stride=2.5, *args, **kwargs):
        self.initial_heading = initial_heading
        self.heading_amplitude = heading_amplitude
        self.stride = stride
        self.initial_pos = np.array(_sub_position()[:2])
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
        self.task()
        if self.task.finished:
            self.new_task()

class VelocityHeadingSearch(Task):
    """
    Search by moving forward and oscillating heading, without using absolute
    positional controls
    """

    def on_first_run(self, initial_heading=None, stride_speed=1, stride_time=1.5, heading_amplitude=45, *args, **kwargs):
        if initial_heading is None:
            initial_heading = shm.desires.heading.get()
        Heading(initial_heading)(),

        self.use_task(While(lambda: Sequential(
            Timed(VelocityX(stride_speed), stride_time), Zero(),
            HeadingSearch.Scan(initial_heading + heading_amplitude),
            HeadingSearch.Scan(initial_heading - heading_amplitude),
            Heading(initial_heading),
        ), True))


def cons(task, total=1*60, success=1*60*0.85, debug=False):
    return ConsistentTask(task, total=total, success=success, debug=debug)


class SaneHeadingSearch(Task):
    def make_turn(self):
        h = self.base_heading + (self.side * 90)
        h %= 360
        print("Turning to {}".format(h))
        return cons(Heading(h))


    def make_move(self):
        t = (self.loop + (self.side == 3)) * 6
        print("Moving for {}".format(t))
        return Timed(VelocityX(0.3), t)


    def on_first_run(self, *args, **kwargs):
        self.base_heading = shm.kalman.heading.get()
        self.side = 0
        self.loop = 1
        self.heading_task = self.make_turn()
        self.heading_task()
        self.movement_task = None
        self.stop_task = VelocityX(0)
        self.state = "h"


    def on_run(self, *args, **kwargs):
        if self.state == "h":
            self.heading_task()
            if self.heading_task.finished:
                self.state = "x"
                self.movement_task = self.make_move()
                self.logw("Turned")
        elif self.state == "x":
            self.movement_task()
            if self.movement_task.finished:
                self.stop_task()
                self.side = (self.side + 1) % 4
                if self.side == 0:
                    self.loop += 1
                self.heading_task = self.make_turn()
                self.state = "h"
                self.logw("Moved")
