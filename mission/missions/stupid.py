# Designed by Zander in California

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
    SaneHeadingSearch,
)
from mission.missions.actuate import (
    FireGreen,
    FireRed,
)

from mission.constants.config import cash_in as settings

from mission.missions.will_common import BigDepth


dist1 = 45
dist2 = 52
dist3 = dist1 / 2


stupid_castor = Sequential(
    BigDepth(1.5),
    Timed(VelocityX(0.4), dist1),
    # MoveXRough(20),
    VelocityX(0),
    Heading(37),
    Timed(VelocityX(0.4), dist2),
    VelocityX(0),
    finite=True
)

stupid_castor_2 = Sequential(
    Heading(10),
    Timed(VelocityX(0.4), dist3),
    VelocityX(0),
    finite=True
)


search = SaneHeadingSearch()
