# POLLUX MASTER MISSION
# CUAUV ROBOSUB 2018

import shm

from mission.framework.combinators import MasterConcurrent, Sequential
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, Log, Zero, NoOp
from mission.framework.targeting import DownwardTarget
from mission.framework.movement import RelativeToCurrentHeading, RelativeToInitialHeading

from mission.missions.will_common import BigDepth, FakeMoveX

from mission.missions.master_common import RunAll, MissionTask, TrackerGetter, TrackerCleanup, DriveToSecondPath

from mission.missions.gate import gate as Gate
from mission.missions.path import get_path as PathGetter
from mission.missions.dice import Full as Dice

from mission.missions.cash_in import norm_to_vision_downward

from mission.constants.region import PATH_1_BEND_RIGHT, PATH_2_BEND_RIGHT
from mission.constants.timeout import timeouts

def time_left():
    # TODO test this?
    #time_in = Master.this_run_time - Master.first_run_time
    return 0 #20 * 60 - time_in

WaitForTime = Sequential(
    Zero(),
    Log('Waiting until five minutes left...'),
    Log('Time left: {}, so waiting {} seconds.'.format(time_left(), max(time_left() - 5 * 60, 0))),
    # Wait until five minutes left
    Timer(max(time_left() - 5 * 60, 0)),
)

CASH_IN_GROUPS = [(group.center_x, group.center_y) for group in [shm.recovery_vision_downward_bin_red, shm.recovery_vision_downward_bin_green]]

cash_in_center = lambda: tuple(sum([val.get() for val in dimen]) for dimen in CASH_IN_GROUPS)

# TODO test this
SurfaceAtCashIn = Sequential(
    Zero(),
    Timer(3),
    Log('Aligning with cash-in'),
    #DownwardTarget(
    #    cash_in_center,
    #    target=norm_to_vision_downward(0, 0),
    #    deadband=norm_to_vision_downward(-0.5, -0.5),
    #    px=0.0005, py=0.001,
    #),
    Log('Surfacing at cash-in!'),
    BigDepth(0),
)

# --------

gate = MissionTask(
    name='Gate',
    cls=Gate,
    modules=[shm.vision_modules.BicolorGate],
    surfaces=False,
    timeout=timeouts['gate'],
)

gate_dead_reckon = MissionTask(
    name='GateDead',
    cls=Sequential(
        Zero(),
        Log('Dead reckoning through gate...'),
        BigDepth(2.2),
        FakeMoveX(dist=5.8, speed=0.2),
    ),
    modules=None,
    surfaces=False,
)

fake_path = lambda bend_right: lambda: MissionTask(
    name='FakePath',
    cls=Sequential(
        FakeMoveX(dist=0.5, speed=0.2),
        RelativeToInitialHeading(45 if bend_right else -45),
    ),
    modules=None,
    surfaces=False,
)

get_path = lambda bend_right: lambda: MissionTask(
    name='Path',
    cls=PathGetter(bend_right),
    modules=[shm.vision_modules.Pipes],
    surfaces=False,
    timeout=timeouts['path'],
    on_timeout=RelativeToInitialHeading(45 if bend_right else -45),
)

dice = MissionTask(
    name='Dice',
    cls=Dice,
    modules=None, #[shm.vision_modules.Dice],
    surfaces=False,
    timeout=timeouts['dice'],
)

highway = MissionTask(
    name='Highway',
    cls=DriveToSecondPath,
    modules=None,
    surfaces=False,
    timeout=timeouts['highway'],
)

wait_for_track = MissionTask(
    name='StuckInTraffic',
    cls=WaitForTime,
    modules=None,
    surfaces=False,
)

track = MissionTask(
    name='Track',
    cls=TrackerGetter(
        # We can't actually find roulette because the vision module is disabled
        found_roulette=NoOp(),
        found_cash_in=NoOp(),
        enable_roulette=False,
    ),
    modules=[shm.vision_modules.CashInDownward],
    surfaces=False,
    on_exit=TrackerCleanup(),
    timeout=timeouts['track'],
)

# get a little closer to cash-in before we start tracking
interlude = lambda dist, speed: lambda: MissionTask(
    name='Interlude',
    cls=Sequential(
        FakeMoveX(dist=dist, speed=speed),
    ),
    modules=None,
    surfaces=False,
)

surface_cash_in = MissionTask(
    name='SurfaceCashIn',
    cls=SurfaceAtCashIn,
    modules=[shm.vision_modules.CashInDownward],
    surfaces=True,
)

# This is used for testing, not used in the actual master mission
TestTrack = Sequential(
    TrackerGetter(
        # found_roulette=FunctionTask(lambda: find_task(ROULETTE)),
        # found_cash_in=FunctionTask(lambda: find_task(CASH_IN)),
        found_roulette=FunctionTask(lambda: False),
        found_cash_in=FunctionTask(lambda: False),
        enable_roulette=True,
        enable_cash_in=True,
    ),
    # TrackCleanup(),
)

tasks = [
    #gate,
    lambda: gate_dead_reckon,
    #get_path(PATH_1_BEND_RIGHT),
    fake_path(PATH_1_BEND_RIGHT),
    interlude(dist=3.0, speed=0.2),
    lambda: dice,
    lambda: highway,
    #get_path(PATH_2_BEND_RIGHT),
    fake_path(PATH_2_BEND_RIGHT),
    interlude(dist=4.0, speed=0.2),
    #wait_for_track,
    track,
    surface_cash_in,
]

Master = RunAll(tasks)
