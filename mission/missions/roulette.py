# Written by Will Smith.

from collections import namedtuple
import math
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
from mission.framework.helpers import call_if_function
from mission.framework.targeting import DownwardTarget, DownwardAlign
from mission.framework.timing import Timer
from mission.framework.movement import (
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
    Depth,
)
from mission.framework.primitive import (
    Zero,
    Log,
    Succeed,
    Fail,
    FunctionTask,
    NoOp,
)
from mission.framework.search import SpiralSearch, VelocitySwaySearch, SearchFor, SaneHeadingSearch
from mission.framework.position import PositionalControl

from mission.missions.actuate import FireBlue, FireRed, FireGreen
from mission.missions.will_common import BigDepth, ForwardSearch

from conf.vehicle import cameras

from mission.constants.config import roulette as settings

# These values are for Teagle
# Perhaps we should instead do this by determining the size in the camera
DEPTH_STANDARD = settings.depth_search
DEPTH_TARGET_ALIGN_BIN = settings.depth_realign
DEPTH_TARGET_DROP = settings.depth_drop

CAM_CENTER = (cameras['downward']['width']/2, cameras['downward']['height']/2)

BIN_CENTER = [shm.bins_vision.center_x, shm.bins_vision.center_y]
#GREEN_CENTER = [shm.bins_green0.centroid_x, shm.bins_green0.centroid_y]
GREEN_CENTER = BIN_CENTER
GREEN_ANGLE = shm.bins_green0.angle

HEADING_OFFSET = settings.heading_offset

align_roulette_center = lambda db=20, p=0.0005: DownwardTarget((BIN_CENTER[0].get, BIN_CENTER[1].get), target=CAM_CENTER, px=p, py=p, deadband=(db, db))
align_green_angle = lambda db=10, p=0.8: DownwardAlign(lambda: GREEN_ANGLE.get() + HEADING_OFFSET, target=0, deadband=db, p=p)

DropBall = lambda: FireBlue()

Search = lambda: SearchFor(
    SaneHeadingSearch(),
    shm.bins_vision.board_visible.get,
    consistent_frames=(1*60, 1.5*60) # multiply by 60 to specify in seconds
)

Full = lambda: Retry(
    lambda: Sequential(
        Log('Starting'),
        Zero(),
        #BigDepth(DEPTH_STANDARD, timeout=8),
        # Disabled search for now because if tracker mission sees the table, then we don't need to search
        Log('Searching for roulette...'),
        Search(),
        Zero(),
        Log('Centering on roulette...'),
        align_roulette_center(db=40, p=0.0005),
        Log('Descending on roulette...'),
        MasterConcurrent(
            BigDepth(DEPTH_TARGET_ALIGN_BIN, timeout=8),
            align_roulette_center(db=0, p=0.0002),
        ),
        Log('Aligning with table again...'),
        align_roulette_center(db=30, p=0.0002),
        Log('Aligning heading with green bin...'),
        MasterConcurrent(
            align_green_angle(db=5, p=0.5),
            align_roulette_center(db=0, p=0.0003),
        ),
        Zero(),
        Log('Aligning with table again again...'),
        MasterConcurrent(
            align_roulette_center(db=50, p=0.0003),
            align_green_angle(db=0, p=0.5),
        ),
        Zero(),
        Log('Descending on table...'),
        # Don't align... when we get too close the readings throw things off
        PositionalControl(True),
        BigDepth(DEPTH_TARGET_DROP, timeout=16),
        Zero(),
        Log('Dropping ball...'),
        DropBall(),
        PositionalControl(False),
        Log('Returning to normal depth...'),
        BigDepth(DEPTH_STANDARD, timeout=8),
        Log('Done'),
    )
, attempts=5)
