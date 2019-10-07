
import shm

import math

# from mission.constants.config import HYDROPHONES_PINGER_DEPTH
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
        print(min((45 - diff) / 20, 0.4))
        return min((45 - diff) / 20, 0.4)
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

class _TrackPinger(Task):

    def on_first_run(self, speed=0.4):
        self.last_shmval = None
        self.last_target_heading = None
        self.last_target_elevation = 0
        self.checker = ConsistencyCheck(count=3, total=7)

    def update(self):
        self.shmval = shm.hydrophones_results_track.tracked_ping_heading.get()
        # print('last: ' + str(self.last_shmval) + ', new: ' + str(self.shmval))
        if self.last_shmval is None or self.shmval != self.last_shmval:
            lltarget_heading = self.last_target_heading if self.last_target_heading is not None else self.shmval
            self.last_shmval = self.shmval
            self.last_target_heading = self.shmval
            self.last_target_elevation = shm.hydrophones_results_track.tracked_ping_elevation.get()
            self.log('heading: ' + str(self.last_target_heading), level='info')
            self.log('elevation: ' + str(self.last_target_elevation), level='info')
            return self.checker.check(self.angle_diff(self.get_target_heading(), lltarget_heading) > 90)
        return False

    def get_target_heading(self):
        return self.last_target_heading

    def get_target_elevation(self):
        return self.last_target_elevation

    def angle_diff(self, a, b):
        return math.degrees(abs(math.atan2(math.sin(math.radians(a) - math.radians(b)),
                                           math.cos(math.radians(a) - math.radians(b)))))

    def calc_speed(self, speed):
        diff = self.angle_diff(self.get_target_heading(), shm.kalman.heading.get())
        print("diff: " + str(diff))
        if diff < 45:
            print(min((45 - diff) / 20, speed))
            return min((45 - diff) / 20, speed)
        else:
            print("0")
            return 0

    def on_run(self, speed):
        if self.update():
            VelocityX(0)()
            self.finish()
            return
        Heading(self.get_target_heading())()
        VelocityX(self.calc_speed(speed))()



TrackPinger = lambda speed=0.4, depth=2.3: Sequential(Depth(depth, error=0.2), _TrackPinger(speed))


test = TrackPinger(0.2)

what = Heading(None)
