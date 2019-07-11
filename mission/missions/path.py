#!/usr/bin/env python3

import math

import shm

from conf.vehicle import VEHICLE

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY, RelativeToCurrentHeading
from mission.framework.position import PositionalControl
from mission.framework.primitive import Zero, Log, FunctionTask, Fail, NoOp
from mission.framework.search import SearchFor, VelocityTSearch, SwaySearch, PitchSearch, VelocitySwaySearch
from mission.framework.targeting import DownwardTarget, PIDLoop, HeadingTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.jank import TrackMovementY, RestorePosY

from mission.constants.config import path as settings
from mission.constants.region import PATH_1_BEND_RIGHT, PATH_2_BEND_RIGHT

from mission.missions.will_common import Consistent, BigDepth, is_mainsub, FakeMoveX
from auv_python_helpers.angles import heading_sub_degrees
import numpy as np

def visible_test(count):
    return lambda: shm.path_results.num_lines.get() >= count

SearchTask = lambda: SearchFor(VelocitySwaySearch(width=settings.search_forward, stride=settings.search_stride, speed=settings.search_speed, rightFirst=settings.search_right_first),
                                visible_test(2),
                                consistent_frames=(60, 90))

class FirstPipeGroupFirst(Task):
    # Checks whether the first pipe group in shm is the first pipe we should follow.
    # Succeeds if the first pipe group is consistently the right one, fails otherwise
    def on_first_run(self, bend_right): 
        self.angle_1_checker = ConsistencyCheck(6, 8)
        self.angle_2_checker = ConsistencyCheck(6, 8)

        
    def on_run(self, bend_right):
        angle_1 = shm.path_results.angle_1.get()
        angle_2 = shm.path_results.angle_2.get()
        v1 = np.complex128([np.exp(1j * angle_1)]).view(np.float64)
        v2 = np.complex128([np.exp(1j * angle_2)]).view(np.float64)

        cp = np.cross(v1, v2)
        cond = (cp > 0) ^ bend_right

        if self.angle_1_checker.check(cond):
            self.finish()
        if self.angle_2_checker.check(not cond):
            self.finish(success=False)
        return

        diff = math.atan(math.sin((angle_2 - angle_1)) / math.cos((angle_2 - angle_1)))

        # TODO this might not be working
        print(angle_1, angle_2, diff)

        if self.angle_1_checker.check(diff > 0 ^ (angle_1 < angle_2) ^ (not bend_right)):
            self.finish()
        if self.angle_2_checker.check(diff < 0 ^ (angle_1 < angle_2) ^ (not bend_right)):
            self.finish(success=False)

#class PipeIdentifier(Task):
#    def on_first_run(self):
#        self.total_1


def s(n):
    b = [' '] * 360
    b[180] = '+'
    b[n] = '#'
    return ''.join(b)
def s2(a, b, c):
    bl = [' '] * 360
    bl[180] = '+'
    bl[a] = 'A'
    bl[b] = 'B'
    bl[c] = 'D'
    return ''.join(bl)
def heading_to_vector(h):
    return np.exp([1j * h]).astype(np.complex128).view(np.float64)
def hwrap(f):
    def _wrap(*args, **kwargs):
        print('c')
        return f(*args, **kwargs)
    return _wrap
DEADBAND = .06
def is_done(heading, trg, dst, deadband):
    v = heading_to_vector(heading()) * -dst
    print(shm.path_results.center_x.get(), shm.path_results.center_y.get(), v)
    return  abs(heading_sub_degrees(trg, heading(), math.pi*2)) < math.radians(5) and \
            abs(shm.path_results.center_x.get() - v[0]) < deadband and \
            abs(shm.path_results.center_y.get() - v[1]) < deadband


PipeAlign = lambda heading, trg, dst, deadband: Sequential(
    Log("PipeAlign start"),
    MasterConcurrent(
        While(
            lambda: Sequential(
                Log("attempt asdasd"),
                Concurrent(
                    DownwardTarget(
                        lambda: (shm.path_results.center_x.get(), shm.path_results.center_y.get()),
                        target=(lambda: heading_to_vector(heading()) * -dst),
                        deadband=(deadband, deadband), px=1, py=1, ix=.05, iy=.05
                    ),
                    While(
                        lambda: Sequential(
                            FunctionTask(lambda: shm.navigation_desires.heading.set((shm.kalman.heading.get()-.95*math.degrees(heading_sub_degrees(trg, heading(), math.pi*2))) % 360) if shm.path_results.num_lines.get() == 2 else None),
                            #Log('{:03d} '.format(int(shm.navigation_desires.heading.get())) + s2(int(math.degrees(trg)), int(math.degrees(heading())), int(shm.desires.heading.get())) if shm.path_results.num_lines.get() == 2 else 'X'),
                            Timer(.05)
                            ),
                        lambda: abs(heading_sub_degrees(trg, heading(), math.pi*2)) > math.radians(5)
                    )#, count=6, total=8, invert=False, result=True),
                )
            ),
            lambda: not is_done(heading, trg, dst, deadband)#lambda: abs(heading_sub_degrees(trg, heading(), math.pi*2)) > math.radians(5) or abs(shm.path_results.center_x.get()) > .08 or abs(shm.path_results.center_y.get()) > .08
        ),

            #, count=3, total=4, invert=False, result=True),
        #While(lambda: Log("V: {} h: {} d: {} x: {} y: {} c: {}".format(shm.path_results.num_lines.get(), heading(), math.degrees(heading_sub_degrees(trg, heading(), math.pi*2)), shm.path_results.center_x.get(), shm.path_results.center_y.get(), heading_to_vector(heading()) * dst)), lambda: True),
        #While(lambda: Log(s(int(math.degrees(heading_sub_degrees(trg, heading(), math.pi*2))) + 180)), lambda: True),
         #While(lambda: Log(s2(int(math.degrees(trg)), int(math.degrees(heading()))) if shm.path_results.num_lines.get() == 2 else 'X'), lambda: True),
    ),
    Log("Centered on Pipe in PipeAlign!"),
    #FunctionTask(lambda: shm.navigation_desires.heading.set(-180/math.pi*heading()+shm.kalman.heading.get()))
)



FollowPipe = lambda h1, h2: Sequential(
    PipeAlign(h1), 
    Zero(),
    Log("Aligned To Pipe!"),
    DownwardTarget(lambda: (shm.path_results.center_x.get(), shm.path_results.center_y.get()),
                   target=(0,0),
                   deadband=(.1, .1), px=0.5, py=0.5),
    Zero(),
    Log("Centered on Pipe!"),
    FunctionTask(lambda: shm.navigation_desires.heading.set(-180/math.pi*h2()+shm.kalman.heading.get())),
    Timer(4),
    Log("Facing new direction!"),
    Zero(),
)

def t180(a): return (a + math.pi) % (2 * math.pi)

FullPipe = lambda bend_right=False: Sequential(
    # Don't do anything stupid
    FunctionTask(lambda: shm.path_results.num_lines.set(0)),
    BigDepth(settings.depth),
    Zero(),
    Log("At right depth!"),
    Retry(
        task_func=lambda: Sequential(
            Log("Searching for path..."),
            SearchTask(),
            Zero(),
            Log("Found Pipe!"),
            Conditional(
                Either(
                    Sequential(
                        # Don't lose sight in the first second
                        Timer(1.0),
                        # Require a really high fail rate - path vision can be finicky
                        Consistent(visible_test(1), count=2.5, total=3, result=False, invert=True),
                    ),
                    Conditional(FirstPipeGroupFirst(bend_right),
                        on_success=Sequential(Log("first group"), FollowPipe(lambda: t180(shm.path_results.angle_1.get()), shm.path_results.angle_2.get)),
                    on_fail=Sequential(Log("second group"), FollowPipe(lambda: t180(shm.path_results.angle_2.get()), shm.path_results.angle_1.get))),
                ),
                on_success=Sequential(
                    Timed(VelocityX(.1), settings.post_dist),
                    Log("Done!"),
                    Zero(),
                    Log("Finished path!"),
                ),
                on_fail=Fail(
                    Sequential(
                        Log("Lost sight of path. Backing up..."),
                        FakeMoveX(-settings.failure_back_up_dist, speed=settings.failure_back_up_speed),
                    ),
                ),
            ),
        ),
        attempts=5
    )
)


path = FullPipe()
def gen_pipe(s, a1, a2):
    t1 = PipeAlign(a1, math.pi/2, .2, DEADBAND * 1.5)
    t2 = PipeAlign(a1, math.pi/2, 0, DEADBAND * 1.5)
    t3 = PipeAlign(a2, -math.pi/2, 0, DEADBAND)
    t4 = PipeAlign(a2, -math.pi/2, .3, DEADBAND)
    return Sequential(
        Log(s),
        Log("t1"),
        t1,
        Timer(.5),
        Log("t2"),
        t2,
        Timer(.15),
        Log("t3"),
        t3,
        Timer(.5),
        Log("t4"),
        t4,
        Timer(.5),
        Log("done")
    )

path2 = Sequential(
    Log("Searching for path..."),
    While(
        lambda: Conditional(
            FunctionTask(lambda: shm.path_results.num_lines.get() in (0, 1)),
            on_success=MasterConcurrent(
                While(lambda: NoOp(), lambda: shm.path_results.num_lines.get() < 2),
                Log("Centering on blob..."),
                DownwardTarget(
                    lambda: (shm.path_results.center_x.get(), shm.path_results.center_y.get()),
                    target=(0, 0),
                    deadband=(.1, .1), px=.5, py=.5, ix=.05, iy=.05
                ),
            ),
            on_fail=Sequential(Log("Blind search..."), SearchTask())
        ),
        lambda: shm.path_results.num_lines.get() < 2
    ),
    Zero(),
    Log("Found Pipe!"),
    Conditional(
        FunctionTask(lambda: math.sin(shm.path_results.angle_1.get()) > math.sin(shm.path_results.angle_2.get())),
        on_success=gen_pipe("a", shm.path_results.angle_1.get, shm.path_results.angle_2.get),
        on_fail=gen_pipe("b", shm.path_results.angle_2.get, shm.path_results.angle_1.get),
    )
)

get_path = lambda bend_right: FullPipe(bend_right)
