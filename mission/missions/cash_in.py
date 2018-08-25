import time
from collections import deque
import itertools

import numpy as np

import shm
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.search import SearchFor, SpiralSearch, VelocitySwaySearch
from mission.framework.combinators import (
    Sequential,
    MasterConcurrent,
    Concurrent,
    Retry,
    Conditional,
    While,
    Defer,
)
from mission.framework.movement import (
    Depth,
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    Pitch,
    Roll,
    VelocityX,
    VelocityY,
    Heading,
    RelativeToInitialHeading,
)
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.primitive import (
    Zero,
    FunctionTask,
    NoOp,
    Log,
    Succeed,
    Fail,
)
from mission.framework.helpers import (
    ConsistencyCheck,
    call_if_function,
    within_deadband,
)
from mission.framework.position import (
    MoveX,
    MoveXRough,
    MoveY,
    MoveYRough,
    MoveXY,
    MoveXYRough,
    GoToPositionRough,
    WithPositionalControl,
    PositionalControl,
)
from mission.framework.targeting import (
    DownwardTarget,
    ForwardTarget,
    HeadingTarget,
    PIDLoop,
)
from mission.framework.track import (
    Matcher,
    Match,
    Observation,
    ComplexColor,
    HeadingInvCameraCoord,
    HeadingInvAngle,
    ConsistentObject
)
from mission.missions.ozer_common import (
    GlobalTimeoutError,
    GradualHeading,
    GradualDepth,
    SearchWithGlobalTimeout,
    CenterCentroid,
    Disjunction,
    ConsistentTask,
    PrintDone,
    Altitude,
    AlignAmlan,
    # AMLANS,
    Infinite,
    Except,
)
from mission.framework.search import (
    SearchFor,
    VelocitySwaySearch,
)
from mission.missions.actuate import (
    FireGreen,
    FireRed,
)

from mission.constants.config import cash_in as settings

def vision_to_norm_downward(x, y=None):
    if y is None:
        x, y = call_if_function(x)
    else:
        x, y = call_if_function(x), call_if_function(y)

    w, h = shm.camera.downward_width.get(), shm.camera.downward_height.get()

    return ((x / w - 0.5) * 2, (y / h - 0.5) * 2)


def norm_to_vision_downward(x, y=None):
    if y is None:
        x, y = call_if_function(x)
    else:
        x, y = call_if_function(x), call_if_function(y)

    w, h = shm.camera.downward_width.get(), shm.camera.downward_height.get()

    return ((x / 2 + 0.5) * w, (y / 2 + 0.5) * h)

def vision_to_norm_forward(x, y=None):
    if y is None:
        x, y = call_if_function(x)
    else:
        x, y = call_if_function(x), call_if_function(y)

    w, h = shm.camera.forward_width.get(), shm.camera.forward_height.get()

    return ((x / w - 0.5) * 2, (y / h - 0.5) * 2)


def norm_to_vision_forward(x, y=None):
    if y is None:
        x, y = call_if_function(x)
    else:
        x, y = call_if_function(x), call_if_function(y)

    w, h = shm.camera.forward_width.get(), shm.camera.forward_height.get()

    return ((x / 2 + 0.5) * w, (y / 2 + 0.5) * h)


def cons(task, total=2.5*60, success=2.5*60*0.85, debug=False):
    return ConsistentTask(task, total=total, success=success, debug=debug)


def stop():
    return Concurrent(
        VelocityX(0),
        VelocityY(0),
    )


def timed(task, timeout=2):
    return Timed(task, timeout)


def VisionSelector(forward=False, downward=False):
    return Sequential(
        FunctionTask(lambda: shm.vision_modules.CashInForward.set(forward)),
        FunctionTask(lambda: shm.vision_modules.CashInDownward.set(downward)),
    )


class ApproachAndTargetFunnel(Task):
    def on_first_run(self, shm_group, *args, **kwargs):
        FUNNEL_DEPTH = settings.approach_funnel_depth

        forward_target_task = ForwardTarget(
            point=(shm_group.center_x.get, shm_group.center_y.get),
            target=norm_to_vision_forward(0.0, 0.4),
            depth_bounds=(.45, 1.0),
            deadband=norm_to_vision_forward(-0.9, -0.9),
            px=0.0004,
            py=0.0008,
            max_out=.05,
        )

        heading_target_task = HeadingTarget(
            point=(shm_group.center_x.get, shm_group.center_y.get),
            target=norm_to_vision_forward(0.0, 0.4),
            depth_bounds=(.45, 1.0),
            deadband=norm_to_vision_forward(-0.9, -0.9),
            px=0.04,
            py=0.0008,
            max_out=(5, 0.05),
        )

        self.use_task(
            Sequential(
                cons(Depth(FUNNEL_DEPTH)),
                cons(
                    heading_target_task,
                    debug=True
                ),
                cons(
                    Concurrent(
                        forward_target_task,
                        PIDLoop(
                            input_value=shm_group.area.get,
                            target=9000,
                            deadband=250,
                            output_function=VelocityX(),
                            reverse=True,
                            p=0.00008,
                            max_out=.05,
                        ),
                        finite=False,
                    ),
                    total=2.5*60,
                    success=2.5*60*0.9,
                    debug=True,
                ),
            )
        )


class DropInFunnel(Task):
    def on_first_run(self, shm_group, is_left, *args, **kwargs):
        APPROACH_DIST = settings.drop_approach_dist
        DVL_FORWARD_CORRECT_DIST = settings.drop_dvl_forward_correct_dist[is_left]
        DVL_ANGLE_CORRECT = settings.drop_heading_correct[is_left]
        APPROACH_FUNNEL_DEPTH = settings.approach_funnel_depth

        turn_task = RelativeToInitialHeading(90 if is_left else -90)
        reset_heading_task = RelativeToInitialHeading(0)
        reset_heading_task()
        # reset_pos_task = MoveXYRough((0, 0))
        # reset_pos_task()
        self.reset_pos_target = None

        drop_task = FireRed if is_left else FireGreen

        def record_pos():
            self.reset_pos_target = (shm.kalman.north.get(), shm.kalman.east.get())

        Y_DIST = -APPROACH_DIST if is_left else APPROACH_DIST

        self.use_task(
            Sequential(
                VisionSelector(forward=True),
                WithPositionalControl(
                    cons(Depth(APPROACH_FUNNEL_DEPTH))
                ),
                ApproachAndTargetFunnel(shm_group),
                stop(),
                FunctionTask(record_pos),
                VisionSelector(downward=True),
                WithPositionalControl(
                    Sequential(
                        Log("Aligned"),
                        Log("Turning..."),
                        cons(turn_task),
                        Log("Surfacing..."),
                        cons(Depth(-.05)),
                        Log("Moving..."),
                        cons(
                            Concurrent(
                                MoveXYRough((DVL_FORWARD_CORRECT_DIST, Y_DIST)),
                                RelativeToInitialHeading(DVL_ANGLE_CORRECT),
                                finite=False
                            ),
                            debug=True),
                        WithPositionalControl(
                            MasterConcurrent(
                                cons(
                                    FunctionTask(
                                        lambda: shm.recovery_vision_downward_red.probability.get() > .5,
                                        finite=False
                                    ),
                                    total=30,
                                    success=10,
                                    debug=True
                                ),
                                VelocityY(.1 * (-1 if is_left else 1)),
                            ),
                            enable=False
                        ),
                        stop(),
                        Log("Over, dropping!..."),
                        drop_task(),
                        # Timer(3),
                        Log("Moving back..."),
                        WithPositionalControl(
                            Sequential(
                                Timed(VelocityY(-.1 * (-1 if is_left else 1)), 2),
                                stop(),
                            ),
                            enable=False
                        ),
                        cons(GoToPositionRough(lambda: self.reset_pos_target[0], lambda: self.reset_pos_target[1]), debug=True),
                        Log("Diving..."),
                        cons(Depth(.5)),
                        Log("Turning back..."),
                        cons(reset_heading_task, debug=True),
                    )
                )
            )
        )


class PickupFromBin(Task):
    def on_first_run(self, shm_group_getter, is_left, *args, **kwargs):
        BOTH_DEPTH = settings.pick_up_both_depth
        SEARCH_DEPTH_1 = settings.pick_up_search_depth_1
        SEARCH_DEPTH_2 = settings.pick_up_search_depth_2
        SEARCH_DEPTH_3 = settings.pick_up_search_depth_3
        START_PICKUP_DEPTH = settings.pick_up_start_follow_depth

        def downward_target_task(pt=(0, 0), deadband=(0.1, 0.1), max_out=0.04):
            return DownwardTarget(
                point=(lambda: shm_group_getter().center_x.get(), lambda: shm_group_getter().center_y.get()),
                target=norm_to_vision_downward(*pt),
                deadband=norm_to_vision_downward(-1.0 + deadband[0], -1.0 + deadband[1]),
                px=0.0005,
                py=0.001,
                max_out=max_out,
            )


        def search_at_depth(depth, msg="", target=(0, 0), deadband=(0.1, 0.1), depth_timeout=20):
            return Sequential(
                timed(cons(Depth(depth)), depth_timeout),
                cons(
                    downward_target_task(target, deadband=deadband),
                    success=0.80 * 2.5 * 60,
                    total=2.5 * 60,
                    debug=True,
                ),
                stop(),
                Log("Found at {} (depth={})".format(msg, depth)),
            )

        bottom_target = (0.8, -0.2) if is_left else (-0.7, -0.4)

        self.use_task(
            Sequential(
                VisionSelector(downward=True),
                # cons(Depth(BOTH_DEPTH)),
                # cons(downward_target_task(max_out=0.1)),
                # stop(),
                # cons(Depth(SEARCH_DEPTH_1)),
                # search_at_depth(SEARCH_DEPTH_1, "top", deadband=(0.1, 0.1), depth_timeout=15),
                # search_at_depth(SEARCH_DEPTH_2, "mid", deadband=(0.05, 0.05), depth_timeout=10),
                search_at_depth(SEARCH_DEPTH_3, "bot", target=bottom_target, deadband=(0.05, 0.05)),
                # search_at_depth(3.5, "bot", target=bottom_target, deadband=(0.05, 0.05)),
                Sequential(
                    *(timed(Depth(depth)) for depth in np.arange(SEARCH_DEPTH_3, START_PICKUP_DEPTH + 0.1, 0.1))
                ),
                timed(
                    Concurrent(
                        Depth(START_PICKUP_DEPTH),
                        Roll(7.5 * (-1 if is_left else 1)),
                        Pitch(-7.5),
                    )
                ),
                Log("PICKING UP??"),
                timed(Depth(START_PICKUP_DEPTH + 0.3)),
                Sequential(
                    *(timed(Depth(depth), timeout=1) for depth in np.arange(START_PICKUP_DEPTH, SEARCH_DEPTH_1 + 0.1, 0.1))
                ),
                timed(
                    cons(
                        Concurrent(
                            Depth(SEARCH_DEPTH_1),
                            Roll(0),
                            Pitch(0),
                        )
                    ),
                    15
                ),
            ),
        )


class ResetDepthPIDs(Task):
    def on_run(self, *args, **kwargs):
        from conf.vehicle import control_settings
        for dof, dof_settings in control_settings.items():
            group = getattr(shm, "settings_" + dof)
            for var, value in dof_settings.items():
                getattr(group, var).set(value)

        self.finish()

class MakeControlGreatAgain2018(Task):
    def on_run(self, *args, **kwargs):
        shm.settings_depth.kP.set(1.25)
        shm.settings_depth.kI.set(0.09)
        shm.settings_depth.kD.set(0.30)
        shm.settings_depth.rD.set(0.30)
        self.finish()

    def on_finish(self, *args, **kwargs):
        self.logw("Depth is now tired of winning")


def SearchBin(shm_group_getter, count=1.5, total=2):
    return Sequential(
        VisionSelector(downward=True),
        SearchFor(
            VelocitySwaySearch(),
            lambda: shm_group_getter().probability.get() >= 0.5,
            consistent_frames=(count * 60, total * 60) # multiple by 60 to specify in seconds
        ),
        stop(),
    )


def make_bin_chooser(is_top_left):
    def ret():
        shm_groups = [shm.recovery_vision_downward_bin_red, shm.recovery_vision_downward_bin_green]
        output1 = shm_groups[0].get()
        output2 = shm_groups[1].get()

        if output2.probability < .5:
            # print("Only one")
            return shm_groups[0]

        x1 = output1.center_x
        x2 = output2.center_x

        if abs(x1 - x2) > 25:
            print("Using X")
            if x1 < x2:
                print(shm_groups[int(not is_top_left)].center_x.get())
                return shm_groups[int(not is_top_left)]
            else:
                print(shm_groups[int(is_top_left)].center_x.get())
                return shm_groups[int(is_top_left)]
        else:
            # print("Using Y")
            y1 = output1.center_y
            y2 = output2.center_y
            if y1 < y2:
                print(shm_groups[int(not is_top_left)].center_x.get())
                return shm_groups[int(not is_top_left)]
            else:
                print(shm_groups[int(is_top_left)].center_x.get())
                return shm_groups[int(is_top_left)]

    return ret



reset = ResetDepthPIDs()
make_control_great_again = MakeControlGreatAgain2018()


approach = ApproachAndTargetFunnel(shm.recovery_vision_forward_red)
# approach_left = ApproachAndTargetFunnel(shm.recovery_vision_forward_red)
# approach_right = ApproachAndTargetFunnel(shm.recovery_vision_forward_red)

drop_left = DropInFunnel(shm.recovery_vision_forward_red, is_left=True)
drop_right = DropInFunnel(shm.recovery_vision_forward_red, is_left=False)

search_left = SearchBin(make_bin_chooser(True))
search_right = SearchBin(make_bin_chooser(False))

pickup_left = PickupFromBin(make_bin_chooser(True), is_left=True)
pickup_right = PickupFromBin(make_bin_chooser(False), is_left=False)

pickup_all = Sequential(
    pickup_left,
    Log("Picked Up Left"),
    Timed(VelocityY(0.2), 2),
    VelocityY(0),
    pickup_right,
    Log("Picked Up Right"),
)

drop_all = Sequential(
    drop_right,
    Log("Dropped Right"),
    drop_left,
    Log("Dropped Left"),
)

do_it_all = Sequential(
    search_left,
    Log("Found"),
    pickup_all,
    Log("Picked Up"),
    cons(Heading(45)),
    Log("Turned to hardcoded heading"),
    drop_all,
    Log("Dropped"),
)


left = make_bin_chooser(True)
right = make_bin_chooser(False)

center_left = cons (
    DownwardTarget(
        point=(lambda: left().center_x.get(), lambda: left().center_y.get()),
        target=norm_to_vision_downward(0, 0),
        deadband=norm_to_vision_downward(-0.9, -0.9),
        px=0.0005,
        py=0.001,
        max_out=0.5,
    ),
    debug=True
)

center_right = cons(
    DownwardTarget(
        point=(lambda: right().center_x.get(), lambda: right().center_y.get()),
        target=norm_to_vision_downward(0, 0),
        deadband=norm_to_vision_downward(-0.9, -0.9),
        px=0.0005,
        py=0.001,
        max_out=0.5,
    ),
    debug=True
)

Full = lambda: Sequential(
    Depth(0),
    Depth(1.5),
    pickup_all,
    Depth(0),
    Depth(1.5),
    FireGreen(),
    FireRed(),
    finite=True
)
