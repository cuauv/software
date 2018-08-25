# Written by Will Smith.

from mission.framework.task import Task
from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either, Defer
from mission.framework.movement import Heading, RelativeToInitialHeading, VelocityX, VelocityY, Depth, RelativeToCurrentHeading, RelativeToCurrentDepth
from mission.framework.primitive import Log, NoOp, Zero, Succeed, Fail, FunctionTask
from mission.framework.targeting import ForwardTarget, HeadingTarget, CameraTarget, PIDLoop
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.helpers import ConsistencyCheck, call_if_function
from mission.framework.search import SearchFor, VelocitySwaySearch

from mission.constants.config import dice as settings

from mission.missions.will_common import BigDepth, ForwardSearch, Consistent, FakeMoveX

import time

import shm

BUOY_DEPTH = settings.depth
MAX_DEPTH = settings.max_depth

CAM_DIM = (shm.camera.forward_width.get(), shm.camera.forward_height.get())
CAM_CENTER = (shm.camera.forward_width.get()/2, shm.camera.forward_height.get()/2)

shm_vars = [shm.dice0, shm.dice1]

# True for HeadingTarget, False for ForwardTarget
HEADING_TARGET = False

buoy_pick_checker = ConsistencyCheck(15, 20, default=0)

def __correct_buoy(num):
    if shm_vars[1].visible.get():
        # We assume that we can see both buoys
        coords = [(var.center_x.get(), var.center_y.get()) for var in shm_vars]
        required_diff = 0.04

        axis = not (abs(coords[1][0] - coords[0][0]) > required_diff)
        return num if coords[1][axis] > coords[0][axis] else int(not num)
    else:
        return 0

def update_correct_buoy(num):
    buoy_pick_checker.check(__correct_buoy(num))

def pick_correct_buoy(num, ret=True):
    if shm_vars[1].visible.get():
        # technically this is wrong if we request the other num
        # but this never actually happens in practice
        return buoy_pick_checker.state
    else:
        return 0

def align_buoy(num, db, mult=1):
    if HEADING_TARGET:
        return HeadingTarget(point=lambda: (shm_vars[pick_correct_buoy(num)].center_x.get(),
                                            shm_vars[pick_correct_buoy(num)].center_y.get()),
                             target=(0, 0), deadband=(db, db), px=5, py=0.5,
                             depth_bounds=(1, MAX_DEPTH))
    else:
        return ForwardTarget(point=lambda: (shm_vars[pick_correct_buoy(num)].center_x.get(),
                                            shm_vars[pick_correct_buoy(num)].center_y.get()),
                             target=(0, 0.1), deadband=(db, db), px=0.08*mult, py=0.08*mult, # y is depth
                             depth_bounds=(1, MAX_DEPTH))

# num here refers to the shm group, not the tracked num
SearchBuoy = lambda num, count, total: Sequential(
    BigDepth(BUOY_DEPTH),
    Either(
        SearchFor(
            ForwardSearch(forward=settings.search_forward, stride=settings.search_stride, speed=settings.search_speed),
            shm_vars[num].visible.get,
            consistent_frames=(count * 60, total * 60) # multiple by 60 to specify in seconds
        ),
        # Time out if we can only see one die
        Sequential(
            Timer(settings.search_default_zero_timeout),
            Consistent(lambda: shm_vars[0].visible.get(),
                       count=count, total=total, result=True, invert=False),
        ),
    ),
)

BackUpUntilVisible = lambda num, speed, timeout: Conditional(
    Defer(
        main_task=Either(
            Sequential(
                # Get at least a little bit away first
                FakeMoveX(dist=-0.3, speed=0.2),
                MasterConcurrent(
                    Consistent(lambda: shm_vars[num].visible.get(),
                               count=1, total=1.5, result=True, invert=False),
                    VelocityX(-speed),
                ),
            ),
            Fail(
                Timer(timeout), # don't back up too far
            ),
        ),
        deferred=Sequential(
            VelocityX(0),
        ),
    ),
    on_success=Succeed(
        Sequential(
            NoOp(),
        ),
    ),
    on_fail=Conditional(
        FunctionTask(lambda: num == 0),
        on_success=Sequential(
            Log('Timed out, searching for buoy again'),
            SearchBuoy(num=num, count=4, total=5),
        ),
        # If this is the second buoy, then don't search again goddamit
        on_fail=NoOp(),
    )
)

# This is the radius of the dots on the die
MIN_DOT_RADIUS = settings.min_dot_radius

BackUp = lambda: Sequential(
    Log('Backing up...'),
    # Should be rammed, back up until we can see both buoys
    # If we see the second buoy then we can see both
    BackUpUntilVisible(num=1, speed=0.08, timeout=settings.rammed_back_up_timeout),
)

reset_heading = RelativeToInitialHeading(0)

RamBuoyAttempt = lambda num: Sequential(
    Log('Ramming buoy {}'.format(num)),
    Succeed(reset_heading),
    Conditional(
        Either(
            Consistent(lambda: shm_vars[pick_correct_buoy(num)].visible.get(),
                       count=0.5, total=1.5, invert=True, result=False),
            Sequential(
                MasterConcurrent(
                    Consistent(lambda: shm_vars[pick_correct_buoy(num)].radius_norm.get() < MIN_DOT_RADIUS,
                               count=0.5, total=1.5, invert=True, result=True),
                    Sequential(
                        Zero(),
                        Succeed(reset_heading),
                        Log('Aligning...'),
                        align_buoy(num=num, db=0.05, mult=5),
                        Log('Driving forward...'),
                        Concurrent(
                            VelocityX(0.08),
                            align_buoy(num=num, db=0, mult=3),
                        ),
                    ),
                ),
                Zero(),
                Succeed(reset_heading),
                Log('Aligning one more time...'),
                align_buoy(num=num, db=0.05, mult=5),
            ),
        ),
        on_success=Sequential(
            Zero(),
            Succeed(reset_heading),
            Log('Ramming buoy...'),
            # Ram buoy
            FakeMoveX(dist=settings.ram_dist, speed=settings.ram_speed),
        ),
        on_fail=Fail(
            # We weren't close enough when we lost the buoy - back up and try again
            Sequential(
                Zero(),
                Succeed(reset_heading),
                Log('Lost sight of buoy, backing up...'),
                BackUpUntilVisible(num=0, speed=0.08, timeout=settings.lost_sight_back_up_timeout), # we only need to see the first buoy
            ),
        ),
    ),
    Zero(),
)

# Perform a task while keeping track of the buoy by updating the consistency checker
TrackBuoy = lambda num, task: MasterConcurrent(
    task,
    FunctionTask(lambda: update_correct_buoy(num)),
    finite=False
)

# Ram either buoy, 0 or 1
RamBuoy = lambda num: Sequential(
    # We track the current buoy while ramming and the next one while backing up
    TrackBuoy(num, Retry(lambda: RamBuoyAttempt(num), attempts=10)),
    TrackBuoy(num + 1, BackUp()),
)

Full = Sequential(
    RelativeToInitialHeading(20),
    Log('Searching for buoys...'),
    SearchBuoy(num=1, count=4, total=5), # if we see the second buoy then we see both
    Succeed(RamBuoy(num=0)),
    RelativeToInitialHeading(10, deadband=5), # sometimes mission hangs here - due to poorly tuned heading
    Succeed(RamBuoy(num=1)),
    RelativeToInitialHeading(-10, deadband=5),
)
