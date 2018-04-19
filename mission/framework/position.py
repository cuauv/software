import shm

from auv_math.math_utils import rotate
from mission.framework.combinators import Concurrent, Defer, Sequential
from mission.framework.movement import RelativeToInitialPositionN, RelativeToInitialPositionE, PositionN, PositionE, Heading, Depth
from mission.framework.task import Task
from mission.framework.primitive import FunctionTask
from shm import kalman

class WithPositionalControl(Task):
    def on_first_run(self, task, enable=True, optimize=False):
        enable_var = shm.navigation_settings.position_controls
        optimize_var = shm.navigation_settings.optimize

        init_enable, init_optimize = enable_var.get(), optimize_var.get()

        def set_shm(enable, optimize):
            enable_var.set(enable)
            optimize_var.set(optimize)

        self.use_task(Defer(
            Sequential(
                FunctionTask(lambda: set_shm(enable, optimize)),
                task,
            ),

            FunctionTask(lambda: set_shm(init_enable, init_optimize)),
        ))

class PositionalControl(Task):
    def on_run(self, enable=True, *args, **kwargs):
        shm.navigation_settings.position_controls.set(enable)
        self.finish()

class MoveXY(Task):
    def on_first_run(self, vector, deadband=0.01, *args, **kwargs):
        delta_north, delta_east = rotate(vector, kalman.heading.get())

        n_position = RelativeToInitialPositionN(offset=delta_north, error=deadband)
        e_position = RelativeToInitialPositionE(offset=delta_east, error=deadband)
        self.use_task(WithPositionalControl(
            Concurrent(n_position, e_position, finite=False),
        ))

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
    def on_first_run(self, north, east, heading=None, depth=None, optimize=False, rough=False, deadband=0.05):
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

        self.use_task(WithPositionalControl(
            Concurrent(
                PositionN(self.north, error=deadband),
                PositionE(self.east, error=deadband),
                Heading(self.heading, error=deadband),
                Depth(self.depth, error=deadband)
            ),
            optimize=optimize,
        ))

def GoToPositionRough(*args, **kwargs):
    return GoToPosition(*args, deadband=0.1, **kwargs)

def NavigationSpeed(task, speed):
    speed_var = shm.navigation_settings.max_speed
    init_speed = speed_var.get()

    return Defer(
        Sequential(
            FunctionTask(lambda: speed_var.set(speed)),
            task,
        ),

        FunctionTask(lambda: speed_var.set(init_speed)),
    )

class CheckDistance(Task):
    """
    Finished once we've traveled too far
    """
    def on_first_run(self, *args, **kwargs):
        self.initial_pos = self.pos()

    def on_run(self, distance, *args, **kwargs):
        if self.dist_sq(self.pos(), self.initial_pos) > distance ** 2:
            self.finish()

    def pos(self):
        return [kalman.north.get(), kalman.east.get()]

    def dist_sq(self, p1, p2):
        return (p2[0] - p1[0]) ** 2 + (p2[1] - p1[1]) ** 2
