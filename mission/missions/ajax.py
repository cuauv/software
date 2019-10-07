# CASTOR MASTER MISSION
# CUAUV ROBOSUB 2018

import shm

from mission.framework.task import Task
from mission.framework.combinators import Sequential, MasterConcurrent, Conditional, Either, Retry
from mission.framework.primitive import FunctionTask, Zero, NoOp, InvertSuccess, Fail
from mission.framework.timing import Timer
from mission.framework.movement import RelativeToCurrentHeading, Depth

from mission.missions.master_common import RunAll, MissionTask  # , TrackerGetter, TrackerCleanup, DriveToSecondPath

from mission.missions.will_common import BigDepth, Consistent, FakeMoveX
from mission.missions.attilus_garbage import PositionMarkers

#from mission.missions.gate import gate_no_spin as Gate
from mission.missions.gate import dead_simple as Gate
# from mission.missions.path import get_path as PathGetter
from mission.missions.vamp_buoy import Full as VampBuoy
from mission.missions.paul_bins import DecideAndPush as Bins
from mission.missions.stake import Full as Stake, SearchBoard, BOARD_DEPTH
from mission.missions.vampire import SearchAnyVampire as SearchVampire
from mission.missions.pinger_tracker import TrackPinger

from mission.constants.timeout import timeouts

markers = PositionMarkers()

# TODO: TIMEOUTS

gate = MissionTask(
    name='Gate',
    cls=Gate,
    modules=[shm.vision_modules.Gate],
    surfaces=False,
    timeout=timeouts['gate'],
)

# get_path = lambda bend_right: lambda: MissionTask(
#     name='Path',
#     cls=PathGetter(bend_right),
#     modules=[shm.vision_modules.Pipes],
#     surfaces=False,
#     timeout=timeouts['path'],
#     on_timeout=RelativeToInitialHeading(45 if bend_right else -45),
# )

# highway = MissionTask(
#     name='Highway',
#     cls=DriveToSecondPath,
#     modules=None,
#     surfaces=False,
#     timeout=timeouts['highway'],
# )

vamp_buoy = MissionTask(
    name='VampBuoy',
    cls=lambda: VampBuoy(),
    modules=[shm.vision_modules.VampBuoy],
    surfaces=False,
    timeout=timeouts['vamp_buoy'],
)


bins = MissionTask(
        name='Bins',
        cls=lambda: Bins(),
        modules=[shm.vision_modules.BinsCover, shm.vision_modules.BinsImage, shm.vision_modules.BinsLever],
        surfaces=True,
        timeout=timeouts['bins'],
)

# track_pinger = lambda: MissionTask(
#         name='Track',
#         cls=lambda: TrackPinger(),
#         modules=[],
#         surfaces=False,
#         timeout=timeouts['track_pinger']
# )

# TODO: Recovery + Pinger
Recovery = None
    # MissionTask(
    # name="SurfaceCashIn",
    # cls=lambda: SurfaceCashIn(),
    # modules=None,
    # surfaces=True,
# )


# Which task have we found at random pinger?

#ROULETTE = 1
#CASH_IN = -1
#found_task = 0

#cash_in_surfaced = False

#def find_task(task):
#    global found_task, cash_in_surfaced
#    found_task = task

#def get_found_task():
#    global cash_in_surfaced

#    if found_task == ROULETTE:
#        cash_in_surfaced = True
#        return roulette
#    else:
#        #found_task == CASH_IN:
#        if cash_in_surfaced:
#            return cash_in
#        else:
#            cash_in_surfaced = True
#            return surface_cash_in

    # print('found_task:', found_task)
    # print('cash_in_surfaced:', cash_in_surfaced)
    # return surface_cash_in

    #else:
    #    return MissionTask(name="Failure", cls=NoOp(), modules=None, surfaces=False)

# pinger task should be a function that returns a task
# TODO: MAKE SURE THIS WORKS
pinger_task = None
pinger_tasks = [Recovery, Stake]

def set_pinger_task(task):
    global pinger_task
    pinger_task = task

def set_second_task_if_possible():
    global pinger_task
    if pinger_task in pinger_tasks:
        for p in pinger_tasks:
            if pinger_task != p:
                pinger_task = p
                return True
        return False

def get_pinger_track():
    global pinger_task
    if pinger_task is None:
        return Fail()
    return pinger_task()

# This is semi psuedocode
def TrackerSearch():
    return \
    Retry(
        Sequential(
            Conditional(FunctionTask(set_second_task_if_possible), on_fail= \
                    Sequential(
                        Depth(BOARD_DEPTH, error=0.2),
                        # PingerTracker goes here
                        Conditional(SearchBoard(), on_success=FunctionTask(lambda: set_pinger_task(Stake)), on_fail= \
                                Sequential(
                                    markers.set('center'),
                                    FunctionTask(lambda: set_pinger_task(Recovery))
                                )
                        )
                    )
                )
            ), attempts=3
    )

track = lambda: MissionTask(
    name="Track",
    cls=TrackerSearch,
    modules=[shm.vision_modules.Vampire, shm.vision_modules.Stake],
    surfaces=False,
    timeout=timeouts['track'],
)

# This is used for testing, not used in the actual master mission
# TestTrack = Sequential(
#     TrackerGetter(
#         # found_roulette=FunctionTask(lambda: find_task(ROULETTE)),
#         # found_cash_in=FunctionTask(lambda: find_task(CASH_IN)),
#         found_roulette=FunctionTask(lambda: False),
#         found_cash_in=FunctionTask(lambda: False),
#         enable_roulette=True,
#         enable_cash_in=True,
#     ),
    # TrackCleanup(),
# )

WeirdPath = lambda: Sequential(
        FakeMoveX(1, .3),
        RelativeToInitialHeading(-45),
        FakeMoveX(1, .3),
    
)

SearchBins = lambda: \
        SearchFor(
            VelocitySwaySearch(stride=2, width=2),
            lambda: shm.bins_status.wolf_visible.get() or shm.bins_status.cover_visible.get() or shm.bins_status.bat_visible.get()
            consistent_frames = (3, 5)
        )

path = lambda: MissionTask(
    name="Path",
    cls=WeirdPath,
    modules=[shm.vision_modules.BinsImage, shm.vision_modules.BinsLever, shm.vision_modules.BinsCover],
    surfaces=False,
    timeout=20,
)

search_bins = lambda: MissionTask(
    name="SearchBins",
    cls=SearchBins,
    modules=[shm.vision_modules.BinsImage, shm.vision_modules.BinsLever, shm.vision_modules.BinsCover],
    surfaces=False,
    timeout= 100
)

tasks = [
    lambda: gate,
    path,
    lambda: vamp_buoy,
    path,
    search_bins,
    lambda: bins
]

Master = RunAll(tasks)
