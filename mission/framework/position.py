import shm

from auv_math.math_utils import rotate
from mission.framework.combinators import Concurrent
from mission.framework.movement import RelativeToInitialPositionN, RelativeToInitialPositionE, PositionN, PositionE, Heading, Depth
from mission.framework.task import Task
from shm import kalman

# TODO Should the below be a decorator for Tasks?
class PositionalControlManager:
    enable_var = shm.navigation_settings.position_controls
    optimize_var = shm.navigation_settings.optimize
    def __init__(self, optimize=None):
        self.initial_value = self.enable_var.get()
        self.optimize = optimize
        self.initial_optimize = self.optimize_var.get()

    def restore(self):
        self.enable_var.set(self.initial_value)
        self.optimize_var.set(self.initial_optimize)

    def set(self, value):
        self.enable_var.set(value)
        if self.optimize is not None:
            self.optimize_var.set(self.optimize)

class PositionalControl(Task):
    def on_run(self, enable=True, *args, **kwargs):
        shm.navigation_settings.position_controls.set(enable)
        self.finish()

class MoveXY(Task):
    def on_first_run(self, vector, deadband=0.01, *args, **kwargs):
        self.pos_con_man = PositionalControlManager()
        self.pos_con_man.set(1)

        delta_north, delta_east = rotate(vector, kalman.heading.get())

        n_position = RelativeToInitialPositionN(offset=delta_north, error=deadband)
        e_position = RelativeToInitialPositionE(offset=delta_east, error=deadband)
        self.motion = Concurrent(n_position, e_position, finite=False)

    def on_run(self, *args, **kwargs):
        self.motion()

        if self.motion.has_ever_finished:
            self.finish()

    def on_finish(self, *args, **kwargs):
        self.pos_con_man.restore()

def MoveXYRough(vector):
    return MoveXY(vector, 0.04)

class MoveAngle(MoveXY):
    def on_first_run(self, angle, distance, deadband=0.01, *args, **kwargs):
        super().on_first_run(rotate((distance, 0), angle), deadband=deadband)

def MoveX(distance, deadband=0.01):
    return MoveAngle(0, distance, deadband)

def MoveY(distance, deadband=0.01):
    return MoveAngle(90, distance, deadband)

def MoveXRough(distance):
    return MoveX(distance, 0.04)

def MoveYRough(distance):
    return MoveY(distance, 0.04)

class GoToPosition(Task):
    def on_first_run(self, north, east, heading=None, depth=None, optimize=False):
        self.north = north
        self.east = east

        if heading is None:
            self.heading = shm.navigation_desires.heading.get()
        else:
            self.heading = heading

        if depth is None:
            self.depth = shm.navigation_desires.depth.get()
        else:
            self.depth = depth

        # Change to positional controls at start.
        self.pos_con_man = PositionalControlManager(optimize=optimize)
        self.pos_con_man.set(1)

        self.task = Concurrent(PositionN(self.north), PositionE(self.east), Heading(self.heading), Depth(self.depth))

    def on_run(self, *args, **kwargs):
        self.task()

        if self.task.finished:
            self.finish()

    def on_finish(self, *args, **kwargs):
        self.pos_con_man.restore()

