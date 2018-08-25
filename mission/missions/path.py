#!/usr/bin/env python3

import math

import shm

from conf.vehicle import VEHICLE

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY, RelativeToCurrentHeading
from mission.framework.position import PositionalControl
from mission.framework.primitive import Zero, Log, FunctionTask, Fail
from mission.framework.search import SearchFor, VelocityTSearch, SwaySearch, PitchSearch, VelocitySwaySearch
from mission.framework.targeting import DownwardTarget, PIDLoop, HeadingTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.jank import TrackMovementY, RestorePosY

from mission.constants.config import path as settings
from mission.constants.region import PATH_1_BEND_RIGHT, PATH_2_BEND_RIGHT

from mission.missions.will_common import Consistent, BigDepth, is_mainsub, FakeMoveX

def visible_test(count):
    return lambda: shm.path_results.num_lines.get() >= count

SearchTask = lambda: SearchFor(VelocitySwaySearch(forward=settings.search_forward, stride=settings.search_stride, speed=settings.search_speed, rightFirst=settings.search_right_first),
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

        diff = math.atan(math.sin((angle_2 - angle_1)) / math.cos((angle_2 - angle_1)))

        # TODO this might not be working
        print(angle_1, angle_2, diff)

        if self.angle_1_checker.check(diff > 0 ^ (angle_1 < angle_2) ^ (not bend_right)):
            self.finish()
        if self.angle_2_checker.check(diff < 0 ^ (angle_1 < angle_2) ^ (not bend_right)):
            self.finish(success=False)

PipeAlign = lambda heading: Concurrent(
    DownwardTarget(lambda: (shm.path_results.center_x.get(), shm.path_results.center_y.get()),
                   target=(0, -.25),
                   deadband=(.1, .1), px=0.5, py=0.5),
    Log("Centered on Pipe!"),
    FunctionTask(lambda: shm.navigation_desires.heading.set(-180/3.14*heading.get()+shm.kalman.heading.get()))
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
    FunctionTask(lambda: shm.navigation_desires.heading.set(-180/3.14*h2.get()+shm.kalman.heading.get())),
    Timer(4),
    Log("Facing new direction!"),
    Zero(),
)

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
                                on_success=FollowPipe(shm.path_results.angle_1, shm.path_results.angle_2),
                                on_fail=FollowPipe(shm.path_results.angle_2, shm.path_results.angle_1)),
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

get_path = lambda bend_right: FullPipe(bend_right)
