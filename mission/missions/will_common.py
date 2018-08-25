import math

from mission.framework.task import Task
from mission.framework.combinators import Sequential, MasterConcurrent, Either
from mission.framework.movement import VelocityX, VelocityY
from mission.framework.movement import Depth
from mission.framework.timing import Timer, Timed
from mission.framework.helpers import ConsistencyCheck, call_if_function

import shm

from conf.vehicle import VEHICLE

def is_mainsub():
    return VEHICLE == 'castor'

def interpolate_list(a, b, steps):
    return [a + (b - a) / steps * i for i in range(1, steps + 1)]

def tasks_from_params(task, params, tup=False):
    return [task(*param if tup else param) for param in params]

def tasks_from_param(task, param, length, tup=False):
    return [task(*param if tup else param) for i in range(length)]

def interleave(a, b):
    return [val for pair in zip(a, b) for val in pair]

Descend = lambda depth, timeout, pause, deadband: Sequential(
    Either(
        Timer(timeout),
        Depth(depth, deadband=deadband),
    ),
    Timer(pause),
)

# This lets us descend in depth steps rather than all at once
class BigDepth(Task):
    def on_first_run(self, depth, largest_step=0.5, timeout=4, pause=0, deadband=0.1):
        init_depth = shm.kalman.depth.get()
        steps = math.ceil(abs(depth - init_depth) / largest_step)
        depth_steps = interpolate_list(init_depth, depth, steps)
        params = [(step, timeout, pause, deadband) for step in depth_steps]
        #self.use_task(Sequential(*interleave(tasks_from_params(Depth, depth_steps), tasks_from_param(Timer, timeout, length=steps))))
        self.use_task(Sequential(*tasks_from_params(Descend, params, tup=True)))

FakeMoveX = lambda dist, speed: Sequential(
    MasterConcurrent(Timer(abs(dist / speed)), VelocityX(-speed if dist < 0 else speed)),
    MasterConcurrent(Timer(0.2), VelocityX(speed if dist < 0 else -speed)),
    MasterConcurrent(Timer(0.2), VelocityX(0)),
)

FakeMoveY = lambda dist, speed: Sequential(
    MasterConcurrent(Timer(abs(dist / speed)), VelocityY(-speed if dist < 0 else speed)),
    MasterConcurrent(Timer(0.2), VelocityY(speed if dist < 0 else -speed)),
    MasterConcurrent(Timer(0.2), VelocityY(0)),
)

# A version of VelocitySwaySearch, but better.
# We use this for minisub because it doesn't drift as much.
class ForwardSearch(Task):
    def make_repeat(self, forward, stride, speed, rightFirst):
        dir = 1 if rightFirst else -1
        self.repeat = Sequential(
            FakeMoveY(stride * speed, speed * dir),
            FakeMoveX(forward * speed, speed),
            FakeMoveY(stride * speed, -speed * dir),
            FakeMoveY(stride * speed, -speed * dir),
            FakeMoveX(forward * speed, speed),
            FakeMoveY(stride * speed, speed * dir),
            # For some reason we seem to drift a constant direction - check later?
#            FakeMoveY(0 if is_mainsub else stride * speed * 0.2, speed * -dir),
        )

    def on_first_run(self, forward=1, stride=1, speed=0.3, rightFirst=True):
        self.make_repeat(forward, stride, speed, rightFirst)

    def on_run(self, forward=1, stride=1, speed=0.3, rightFirst=True):
        self.repeat()
        if self.repeat.finished:
            self.make_repeat(forward, stride, speed, rightFirst)

class Consistent(Task):
    def on_first_run(self, test, count, total, invert, result):
        # Multiply by 60 to specify values in seconds, not ticks
        self.checker = ConsistencyCheck(count*60, total*60, default=False)

    def on_run(self, test, count, total, invert, result):
        test_result = call_if_function(test)
        if self.checker.check(not test_result if invert else test_result):
            self.finish(success=result)
