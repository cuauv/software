
import shm

import math

from mission.constants.config import HYDROPHONES_PINGER_DEPTH
from mission.constants.region import PINGER_FREQUENCY, TRACK_MAG_THRESH, TRACK_COOLDOWN_SAMPLES

from mission.framework.task import Task
from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either, Defer
from mission.framework.movement import Heading, RelativeToInitialHeading, VelocityX, VelocityY, Depth, RelativeToCurrentHeading, RelativeToCurrentDepth
from mission.framework.primitive import Log, NoOp, Zero, Succeed, Fail, FunctionTask
from mission.framework.targeting import ForwardTarget, HeadingTarget, CameraTarget, PIDLoop
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.helpers import ConsistencyCheck, call_if_function
from mission.framework.search import SearchFor, VelocitySwaySearch

# positive offset in [0,2pi)
HEADING_OFFSET = 0 #math.pi * 3 / 2

last_shmval = None
last_target_heading = None
last_target_elevation = 0

def update():
    global last_shmval, last_target_heading

    shmval = shm.hydrophones_results_track.tracked_ping_heading.get()
    print('last: ' + str(last_shmval) + ', new: ' + str(shmval))
    if last_shmval is None or shmval != last_shmval:
        last_shmval = shmval
        last_target_heading = shmval
        last_target_elevation = shm.hydrophones_results_track.tracked_ping_elevation.get()
        print('heading: ' + str(last_target_heading))
        print('elevation: ' + str(last_target_elevation))

def get_target_heading():
    return last_target_heading

def get_target_elevation():
    return last_target_elevation

def angle_diff(a, b):
    return math.degrees(abs(math.atan2(math.sin(math.radians(a) - math.radians(b)),
                                       math.cos(math.radians(a) - math.radians(b)))))

def calc_speed():
    diff = angle_diff(get_target_heading(), shm.kalman.heading.get())
    print("diff: " + str(diff))
    if diff < 45:
        print(min((45 - diff) / 20, 0.6))
        return min((45 - diff) / 20, 0.6)
    else:
        print("0")
        return 0

def enable_hydrophones():
    shm.hydrophones_settings.enabled.set(1)

track = Sequential(
    # FunctionTask(enable_hydrophones),
    Depth(1),
    Concurrent(
        While(lambda: FunctionTask(update), lambda: True),
        While(lambda: Heading(get_target_heading), lambda: True),
        While(lambda: VelocityX(calc_speed), lambda: True),
    )
)
