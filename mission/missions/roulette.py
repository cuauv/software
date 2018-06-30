# Written by Will Smith.

from collections import namedtuple
#from math import atan2
import time
import shm
from mission.framework.task import Task
from mission.framework.combinators import (
    Sequential,
    Concurrent,
    MasterConcurrent,
    Retry,
    Conditional,
    While
)
#from mission.framework.helpers import call_if_function
from mission.framework.targeting import DownwardTarget, DownwardAlign
from mission.framework.timing import Timer
from mission.framework.movement import (
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
    Depth,
)
#from mission.framework.position import MoveY
from mission.framework.primitive import (
    Zero,
    Log,
    Succeed,
    Fail,
    FunctionTask,
    NoOp,
)
#from mission.framework.track import ConsistentObject

from conf.vehicle import cameras

# class AlignAndDropBall(Task):
#     """ Waits at the center of the roulette wheel until the target bin is in a predetermined
#     dropping position, and then moves towards the bin to drop a ball """

#     BIN_ANGLE_ALIGNMENT_THRESHOLD = 5 * 3.14 / 180  # 5 degrees
#     BIN_ANGLE_TARGET = 0  # 0 degrees (right)

#     def __init__(self, target_bin, target_angle=0):
#         super().__init__()
#         self.target_bin = target_bin
#         self.target_angle = target_angle
#         self.prereqs = [Predicate(shm.bins_vision.board_visible.get, LocateBoard())]
#         self.prereqs_satisfied = False
#         self.center_task = CenterBoard()
#         self.drop_task = Sequential(Concurrent(RelativeToCurrentDepth(1), MoveY(1)),
#                                     DropBall(),
#                                     Concurrent(RelativeToCurrentDepth(-1), MoveY(-1)))
#         self.task = Sequential(While(self.center_task, self.bin_out_of_position),
#                                self.drop_task)

#     def on_run(self, *args, **kwargs):
#         if not self.prereqs_satisfied:
#             for prereq in self.prereqs:
#                 if not call_if_function(prereq.condition):
#                     prereq.action()
#                     break
#             else:
#                 self.prereqs_satisfied = True
#                 self.task()
#         else:
#             self.finish()


#     def bin_out_of_position(self):
#         if not shm.bins_vision.board_visible.get()\
#            or not self.target_bin.visible.get()\
#            or not self.target_bin.predicted_location.get():
#             return False
#         center_x = shm.bins_vision.center_x.get()
#         center_y = shm.bins_vision.center_y.get()
#         target_bin_x = self.target_bin.predicted_x.get()
#         target_bin_y = self.target_bin.predicted_y.get()
#         diff_x = target_bin_x - center_x
#         diff_y = target_bin_y - center_y
#         bin_angle = atan2(diff_y, diff_x)
#         return abs(bin_angle - self.BIN_ANGLE_TARGET) < self.BIN_ANGLE_ALIGNMENT_THRESHOLD

# We have three dropper mechanisms
PISTONS = {
    'green': (shm.actuator_desires.trigger_11, shm.actuator_desires.trigger_01),
    'red': None,
    'gold': None,
}

# Seconds
PISTON_DELAY = 1

class DropBall(Task):
    def __init__(self, target_piston):
        super().__init__()
        self.target_piston = target_piston

    def on_run(self, *args, **kwargs):
        self.target_piston[0].set(1)
        self.target_piston[1].set(0)

        time.sleep(PISTON_DELAY)

        self.target_piston[0].set(0)
        self.target_piston[1].set(1)

        time.sleep(PISTON_DELAY)

        # Reset
        self.target_piston[1].set(0)

        self.finish()

#5 These values are for Teagle
# Perhaps we should instead do this by determining the size in the camera
DEPTH_STANDARD = 0.8
DEPTH_TARGET_ALIGN_BIN = 2.5
DEPTH_TARGET_DROP = 2.6

CAM_CENTER = (cameras.downward.width/2, cameras.downward.height/2)

def interpolate_list(a, b, steps):
    return [a + (b - a) / steps * i for i in range(steps)]

def tasks_from_params(task, params):
    return [task(param) for param in params]

def tasks_from_param(task, param, length):
    return [task(param) for i in range(length)]

def interleave(a, b):
    # https://stackoverflow.com/a/7946825
    return [val for pair in zip(a, b) for val in pair]

DEPTH_STEPS = interpolate_list(DEPTH_STANDARD, DEPTH_TARGET_ALIGN_BIN, 4)

BIN_CENTER = [shm.bins_vision.center_x, shm.bins_vision.center_y]
#GREEN_CENTER = [shm.bins_green0.centroid_x, shm.bins_green0.centroid_y]
GREEN_CENTER = BIN_CENTER
#GREEN_ANGLE = shm.bins_green0.angle

negator = lambda fcn: -fcn()

align_roulette_center = lambda db: DownwardTarget((BIN_CENTER[0].get, BIN_CENTER[1].get), target=CAM_CENTER, px=0.6, py=0.6, deadband=(db, db))
#align_green_angle = lambda: DownwardAlign(GREEN_ANGLE, target=0)

# TODO
Bin = namedtuple('Bin', ['shm'])

Full = Retry(
    lambda: Sequential(
        Log('Starting'),
        Zero(),
        Depth(DEPTH_STANDARD),
        Log('Centering on roulette'),
        align_roulette_center(0.01),
        Log('Descending on roulette'),
        MasterConcurrent(
            # Descend slowly, not all at once
            Sequential(*interleave(tasks_from_params(Depth, DEPTH_STEPS), tasks_from_param(Timer, 1, len(DEPTH_STEPS)))),
            align_roulette_center(0.000001),
        ),
        Log('Aligning with green bin'),
        align_roulette_center(0.01),
        Log('Descending on green bin'),
        MasterConcurrent(
            Depth(DEPTH_TARGET_DROP),
            align_roulette_center(0.000001),
        ),
        #Log('Aligning heading'),
        #MasterConcurrent(
        #    align_green_angle(),
        #    align_roulette_center(0.000001),
        #),
        Log('Dropping ball'),
        DropBall(PISTONS['green']),
        Log('Returning to normal depth'),
        Depth(DEPTH_STANDARD),
    )
, attempts=5)

Dropper = DropBall(PISTONS['green'])
