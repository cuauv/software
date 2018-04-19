import shm
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.targeting import PIDLoop
from mission.framework.combinators import (
    Sequential,
    Concurrent,
    MasterConcurrent,
    Retry,
    Conditional,
    Defer,
)
from mission.framework.movement import (
    Depth,
    Heading,
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
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
from mission.framework.actuators import FireActuator
from mission.framework.position import MoveX, MoveXY, PositionalControl, MoveXYRough
from mission.framework.track import (
    Matcher,
    Match,
    Observation,
    HeadingInvCameraCoord,
)
from mission.missions.ozer_common import(
    ConsistentTask,
    CenterCentroid,
    AlignHeadingToAngle,
    SearchWithGlobalTimeout,
    Except,
    GlobalTimeoutError,
    Altitude,
    AlignAmlan,
    AMLANS,
    Zeroed,
)
from mission.constants.region import WALL_TOWER_HEADING

def Full():
    return Sequential(
        Log('Doing balls! RIP octagon'),

        Log('Turning away from wall'),
        Heading(WALL_TOWER_HEADING, error=5),

        Log('Surfacing'),
        Depth(0.5, error=0.1),
        Timed(Depth(0), 3),

        Log('Going over to press octagon'),
        Timed(VelocityX(0.3), 11),

        Log('Pulling octagon underwater'),
        Timed(Concurrent(
            VelocityX(0.3),
            Depth(0.5),
            finite=False,
        ), 5),

        Log('Snapping octagon back in place'),
        Timed(VelocityX(-0.2), 4),
        VelocityX(0),

        Log('Well, I guess that\'s it for balls.'),
    )
