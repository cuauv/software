from mission.framework.primitive import (
        Zero,
        Log,
        AlwaysLog,
        Succeed,
        Fail,
        FunctionTask,
        NoOp
)
from mission.framework.combinators import (
        Sequential,
        Concurrent,
        MasterConcurrent,
        Retry,
        Conditional,
        While
)
from mission.framework.targeting import ForwardTarget, PIDLoop, HeadingTarget
from mission.framework.task import Task
from mission.framework.movement import VelocityY, VelocityX
from mission.framework.position import MoveX
from mission.framework.search import SearchFor, SwaySearch, VelocitySwaySearch, MoveX

from mission.missions.will_common import Consistent
from mission.missions.attilus_garbage import PIDStride, PIDSway
from mission.missions.poly import polygon

from mission.framework.timing import Timer, Timed, Timeout

import shm

CAM_CENTER = (shm.vamp_buoy_results.camera_x.get(), shm.vamp_buoy_results.camera_y.get())

TRIANGLE = ("vetalas", "draugr", "aswang")

single = "jiangshi"

CALL = "draugr"

SIZE_THRESH = 8000

last_visible = None

def call_buoy_center():
    return (getattr(shm.vamp_buoy_results, "%s_center_x"%CALL).get(), getattr(shm.vamp_buoy_results, "%s_center_y"%CALL).get())

def any_buoy_center():
    b = which_buoy_visible()
    return (getattr(shm.vamp_buoy_results, "%s_center_x"%b).get(), getattr(shm.vamp_buoy_results, "%s_center_y"%b).get())

def single_buoy_center():
    return (getattr(shm.vamp_buoy_results, "%s_center_x"%single).get(), getattr(shm.vamp_buoy_results, "%s_center_y"%single).get())

def _which_buoy_visible():
    for b in TRIANGLE:
        if getattr(shm.vamp_buoy_results, "%s_visible"%b).get():
            last_visible = b
            return b

def which_buoy_visible():
    global last_visible
    b = _which_buoy_visible()
    if b is not None: last_visible=b
    return last_visible

def call_buoy_visible():
    return _which_buoy_visible() == CALL

def single_buoy_visible():
    return getattr(shm.vamp_buoy_results, "%s_visible"%single).get() 

def call_buoy_size():
    return getattr(shm.vamp_buoy_results, "%s_size"%CALL).get()

def single_buoy_size():
    return getattr(shm.vamp_buoy_results, "%s_size"%single).get()

def any_buoy_size():
    b = which_buoy_visible()
    return getattr(shm.vamp_buoy_results, "%s_size"%b).get()

def align_call_h():
    return getattr(shm.vamp_buoy_results, "%s_align_h"%CALL).get()    

def align_single_h():
    return getattr(shm.vamp_buoy_results, "%s_align_h"%single).get()    

def align_any_h():
    b = which_buoy_visible()
    return getattr(shm.vamp_buoy_results, "%s_align_h"%b).get()    

def triangle_visible():
    for b in TRIANGLE:
        if getattr(shm.vamp_buoy_results, "%s_visible"%b).get():
            return True
    return False


last_seen = "draugr"
def set_last_seen():
    global last_seen
    last_seen = which_buoy_visible()
    return True
def get_sway_direction():
    global last_seen
    return not (getattr(shm.vamp_buoy_results, "%s_center_x"%last_seen).get() < CAM_CENTER[0])


# Sway search but without moving forward
def SwayOnlySearch(speed=0.3, width=2.5, right_first=True):
    direction = 1 if right_first else -1
    return Sequential( 
            Timed(VelocityY(direction*speed), width/(2*speed)),
            Timed(VelocityY(-direction*speed), width/(speed)),
            Timed(VelocityY(direction*speed), width/(2*speed)),
            Zero())

# Search for buoy using SwayOnlySearch
TinySearch = lambda backspeed=0.2, backtime=3: Sequential(
        Zero(),
        Log('Doing TinySearch to see if we can find called'),
        Timeout(SearchFor(
            SwayOnlySearch(right_first=get_sway_direction()),
            call_buoy_visible,
            consistent_frames=(0.5*60, 1.0*60)
            ), 20),
        FunctionTask(set_last_seen),
        Zero(),
)


# Back up, find the triangle buoy again and use it to find the called side
ReSearch = lambda: Sequential(
        SearchTriangleOnFail(),
        AlignAnyNormal(),
        SearchCalled())


# Decorator that wraps a task to search for the called side of the buoy if it fails
withReSearchCalledOnFail = lambda task: lambda: Retry(lambda: \
        Conditional(main_task=task(), on_fail= \
            Fail(Conditional(main_task=TinySearch(), on_fail= \
                Fail(Conditional(main_task=ReSearch(), on_fail=RamAnything()))))), attempts=2)


# The final fallback case. If the called side cannot be found, attempt to ram any side of the triangular buoy if possible.
RamAnything = lambda backspeed=0.2, backtime=10: Sequential(
        Log('Failed, backing up'),
        Timed(VelocityX(-backspeed), backtime), 
        Zero(),
        Timeout(SearchTriangle(), 200),
        AlignAnyNormal(),
        ApproachAny(),
        RamV())

# Decorator that wraps a task to Ram Anything if it fails
withRamAnythingOnFail = lambda task: lambda: Conditional(main_task=Timeout(task(), 100), on_fail=RamAnything())

# Backs up and search for the triangular buoy again
SearchTriangleOnFail = lambda backspeed=0.2, backtime=10: Sequential(
        Log('Failed, backing up'),
        Zero(),
        Timed(VelocityX(-backspeed), backtime), 
        Zero(),
        Timeout(SearchTriangle(), 120))

SearchSingleOnFail = lambda backspeed=0.2, backtime=10: Sequential(
        Log('Failed, backing up'),
        Zero(),
        Timed(VelocityX(-backspeed), backtime), 
        Zero(),
        Timeout(SearchSingle(), 120))

# Decorator that wraps a task to search for the triangular buoy on fail
withSearchTriangleOnFail = lambda task: lambda: Retry(lambda: Conditional(main_task=task(), on_fail=Fail(SearchTriangleOnFail())), attempts=2)

withSearchSingleOnFail = lambda task: lambda: Retry(lambda: Conditional(main_task=task(), on_fail=Fail(SearchSingleOnFail())), attempts=2)

# Decorator that wraps a task to align to the buoy on fail
# Usually the task fails if it loses sight of the buoy, which means Align automatically fails and so search is run
withAlignAnyOnFail = lambda task: lambda: Retry(lambda: Conditional(main_task=task(), on_fail=Fail(AlignAnyNormal())), attempts=2)

withAlignSingleOnFail = lambda task: lambda: Retry(lambda: Conditional(main_task=task(), on_fail=Fail(AlignSingleNormal())), attempts=2)




SEARCH_SIZE_THRESH = 3000
# Task that searches for the triangular buoy
SearchTriangle = lambda stride=2: Sequential( #stride=1.25
        Log('Searching for triangular buoy'),
        SearchFor(
            #SwaySearch(2.0, 0.7),
            VelocitySwaySearch(stride=stride, width=2.5, rightFirst=get_sway_direction()),
            lambda: triangle_visible() and any_buoy_size() > SEARCH_SIZE_THRESH,
            consistent_frames=(1.7*60, 2.0*60) #TODO: Check consistent frames
            ),
        FunctionTask(set_last_seen),
        Log('Finish Search'),
        Zero()
)


# Search for the called buoy by "circling" the buoy with a hexagon
@withRamAnythingOnFail
def SearchCalled():
    return Sequential(
        Log('Searching for called buoy'),
        Zero(),
        SearchFor(
            polygon,
            call_buoy_visible,
            consistent_frames=(1.7*60, 2.0*60) #TODO: Check consistent frames
            ),
        FunctionTask(set_last_seen),
        Zero()
    )



# Point = lambda px=0.3, py=0.0003, d=0.0005, db=0: Concurrent(
#             HeadingTarget(point=any_buoy_center, target=CAM_CENTER, px=px, py=py, dy=d, dx=d, deadband=(db,db)),
#             AlwaysLog(lambda: "center: {}, target: {}".format(CAM_CENTER, any_buoy_center())))


close_to = lambda point1, point2, dbx=20, dby=20: abs(point1[0]-point2[0]) < dbx and abs(point1[1]-point2[1]) < dby
aligned = lambda align, db=3: abs(align) < db

# Aligns horizontally with the buoy. It circles around the buoy using heading target and moving left/right relative to heading
def Align(centerf, alignf, visiblef, px=0.24, py=0.006, p=0.05, d=0.005, dx=0.3, dbx=20, dby=20, db=0): 
    return MasterConcurrent(
            Consistent(lambda: close_to(centerf(), CAM_CENTER, dbx, dby) and aligned(alignf()), count=2.5, total=3.0, invert=False, result=True),
            Consistent(visiblef, count=2.5, total=3.0, invert=True, result=False),
            HeadingTarget(point=centerf, target=CAM_CENTER, px=px, py=py, dy=d, dx=dx, deadband=(db,db)),
            PIDSway(alignf, p=p, d=d, db=db),
            AlwaysLog(lambda: "align_h: %d"%(alignf(),))) 

# Align with any side of the triangular buoy
@withSearchTriangleOnFail
def AlignAnyNormal(): 
    return Align(centerf=any_buoy_center, alignf=align_any_h, visiblef=triangle_visible)

# Align only with the called side
@withReSearchCalledOnFail 
def AlignCalledNormal():
    return Align(centerf=call_buoy_center, alignf=align_call_h, visiblef=call_buoy_visible)

# Align with the single target buoy
@withSearchSingleOnFail
def AlignSingleNormal():
    return Align(centerf=single_buoy_center, alignf=align_single_h, visiblef=single_buoy_visible, px=0.24, py=0.004, dx=0.3, dby=30)


# Centers the buoy using forward target
CenterBuoy = lambda centerf, visiblef, px=0.007, py=0.006, d=0.005, db=0: MasterConcurrent(
            Consistent(lambda: close_to(centerf(), CAM_CENTER), count=2.7, total=3.0, invert=False, result=True),
            #Consistent(visiblef, count=0.2, total=0.3, invert=True, result=False),
            ForwardTarget(point=centerf, target=CAM_CENTER, px=px, py=py, dx=d, dy=d, deadband=(db,db)), 
            AlwaysLog(lambda: "center: {}, target: {}".format(CAM_CENTER, centerf())))

# Centers any side of the triangular buoy
CenterAnyBuoy = lambda: CenterBuoy(centerf=any_buoy_center, visiblef=triangle_visible)
# Centers only the called side
CenterCalledBuoy = lambda: CenterBuoy(centerf=call_buoy_center, visiblef=call_buoy_visible)
# Centers the single target buoy
CenterSingleBuoy = lambda: CenterBuoy(centerf=single_buoy_center, visiblef=single_buoy_visible)


# Approaches a the buoy until it reaches a predetermined size threshold
Approach = lambda sizef, centerf, visiblef: Sequential(
            MasterConcurrent(Consistent(lambda: sizef() > SIZE_THRESH, count=0.2, total=0.3, invert=False, result=True), #ADD EITHER LOSE SIGHT OF BUOY
                Consistent(visiblef, count=2.5, total=3.0, invert=True, result=False),
                Succeed(VelocityX(.2)),
                While(lambda: CenterBuoy(centerf=centerf, visiblef=visiblef), True),
                AlwaysLog(lambda: "size: {}, visible: {}".format(sizef(), visiblef()))),
            Zero())

# Approach only the called buoy
@withReSearchCalledOnFail
def ApproachCalled(): 
    return Approach(sizef=call_buoy_size, centerf=call_buoy_center, visiblef=call_buoy_visible)

# Approach any side of the triangular buoy
@withAlignAnyOnFail
def ApproachAny():
    return Approach(sizef=any_buoy_size, centerf=any_buoy_center, visiblef=triangle_visible)

# Approach the single target buoy
@withAlignSingleOnFail
def ApproachSingle(): 
    return Approach(sizef=single_buoy_size, centerf=single_buoy_center, visiblef=single_buoy_visible)


# Don't use this because we are running the mission on minisub
Ram = lambda: Sequential(Concurrent(AlignAnyNormal(), MoveX(1)), Zero())

# Ram the buoy by approaching it until it is decently sized then moving forward a set amount of time
BIG_SIZE_THRESH = 30000
RamV = lambda: Sequential(Log('Ramming!'), 
        MasterConcurrent(
            Consistent(lambda: any_buoy_size() > BIG_SIZE_THRESH, count=0.2, total=0.3, invert=False, result=True), 
            Succeed(CenterAnyBuoy()), 
            Succeed(VelocityX(.3))), 
        Log('yeet'),
        Timed(VelocityX(0.3), 8),
        Zero())

RamVSingle = lambda: Sequential(Log('Ramming!'), 
        MasterConcurrent(
            Consistent(lambda: single_buoy_size() > BIG_SIZE_THRESH, count=0.2, total=0.3, invert=False, result=True), 
            Succeed(CenterSingleBuoy()), 
            Succeed(VelocityX(.3))), 
        Log('yeet'),
        Timed(VelocityX(0.3), 8),
        Zero())

# Search then approach the buoy
SearchAndApproach = lambda: Sequential(SearchCalled(), AlignCalledNormal(), ApproachCalled())# , AlignCalledNormal())


# The full mission for the triangular buoy
TriangleOnly = lambda: Sequential(
        Log('Searching for buoy'),
        Timeout(SearchTriangle(), 250),
        Log('Found buoy, aligning'),
        AlignAnyNormal(),
        Log('Approaching buoy'),
        ApproachAny(),
        # Log('Aligning again'),
        # AlignAnyNormal(),
        Log('Searching for face'),
        Conditional(FunctionTask(lambda: which_buoy_visible() == CALL), on_fail=SearchAndApproach()),
        Log('Face found, ramming'),
        RamV(),
        Log('Vamp_Buoy Complete')
     )

# Search task for the single target buoy
SearchSingle = lambda: Sequential(
        Log('Searching for singular buoy'),
        SearchFor(
            VelocitySwaySearch(width=2.5, stride=2),
            lambda: shm.vamp_buoy_results.jiangshi_visible.get() and single_buoy_size() > SEARCH_SIZE_THRESH,
            consistent_frames=(1.7*60, 2.0*60) 
            ),
        Zero()
)

# The full mission for the single target buoy
# TODO: Edge cases
SingleOnly = Sequential(
                SearchSingle(),
                AlignSingleNormal(),
                ApproachSingle(),
                RamVSingle()
            )

Full = lambda backtime=20, backspeed=0.2: Sequential(SingleOnly(), Timed(VelX(-backspeed), backtime), TriangleOnly())
