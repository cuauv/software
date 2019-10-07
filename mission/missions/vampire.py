import shm

from mission.framework.search import SearchFor, SwaySearch, SpiralSearch
from mission.framework.combinators import Sequential, MasterConcurrent, While, Conditional
from mission.framework.primitive import Zero, Log, AlwaysLog, FunctionTask, Fail, Succeed
from mission.framework.targeting import DownwardTarget
from mission.framework.movement import Depth, RelativeToInitialDepth, RelativeToCurrentDepth, VelocityY
from mission.framework.position import MoveY
from mission.framework.timing import Timeout, Timer, Timed
from mission.framework.actuators import FireActuator, SetActuators

from mission.missions.will_common import Consistent
from mission.missions.attilus_garbage import PIDHeading, CheckedTimer, DualConsistency, MoveNE, SetMarker, UnsetMarker, GoToMarker

INITIAL_DEPTH_TEAGLE = 1.8
DEPTH_TEAGLE = 2.5
SEARCH_DEPTH_TEAGLE = 1
INITIAL_DEPTH_TRANSDECK = None
DEPTH_TRANSDECK = None
SEARCH_DEPTH_TRANSDECK = None

INITIAL_DEPTH = INITIAL_DEPTH_TEAGLE
SEARCH_DEPTH = SEARCH_DEPTH_TEAGLE
DEPTH = DEPTH_TEAGLE, Conditional
DESCEND_DEPTH = .3

SIZE_THRESH = 9000

CAM_CENTER = shm.recovery_vampire.cam_x.get(), shm.recovery_vampire.cam_y.get()


# TODO: Search Depth
# TODO: Search Empty Circle After Grab

def visible_closed():
    return shm.recovery_vampire.closed_visible.get()
def center_closed():
    return (shm.recovery_vampire.closed_handle_x.get(), shm.recovery_vampire.closed_handle_y.get())
def offset_closed():
    return (shm.recovery_vampire.closed_offset_x.get(), shm.recovery_vampire.closed_offset_y.get())
def direction_closed():
    return shm.recovery_vampire.closed_handle_direction.get()
def angle_offset_closed():
    return shm.recovery_vampire.closed_angle_offset.get()
def size_closed():
    return shm.recovery_vampire.closed_size.get()

def visible_open():
    return shm.recovery_vampire.open_visible.get()
def center_open():
    return (shm.recovery_vampire.open_handle_x.get(), shm.recovery_vampire.open_handle_y.get())
def offset_open():
    return (shm.recovery_vampire.open_offset_x.get(), shm.recovery_vampire.open_offset_y.get())
def angle_offset_open():
    return shm.recovery_vampire.open_angle_offset.get()
def size_open():
    return shm.recovery_vampire.open_size.get()

def center_empty():
    return (shm.recovery_vampire.empty_offset_x.get(), shm.recovery_vampire.empty_offsest_y.get())
def visible_empty():
    return shm.recovery_vampire.empty_visible.get()

last_visible = False

visibles = ['closed', 'open']
def which_visible():
    global last_visible
    for v in visibles:
        if getattr(shm.recovery_vampire, '%s_visible' % v).get():
            last_visible = v
            return v
    return last_visible


def any_visible():
    for v in visibles:
        if getattr(shm.recovery_vampire, '%s_visible' % v).get():
            return True
    return False


def center_any():
    if which_visible():
        return getattr(shm.recovery_vampire, '%s_handle_x' % which_visible()).get(), getattr(shm.recovery_vampire, '%s_handle_y' % which_visible()).get()



Search = lambda visiblef: Sequential(  # TODO: TIMEOUT?
            Log('Searching'),
            SearchFor(
                Sequential(
                    Depth(SEARCH_DEPTH, error=0.2),
                    SpiralSearch(),
                ),
                visiblef,
                consistent_frames=(15, 19)
            ),
            Zero(),
            Depth(INITIAL_DEPTH, error=0.2))

SearchAnyVampire = lambda: Search(any_visible)

close_to = lambda point1, point2, dbx=20, dby=20: abs(point1[0]-point2[0]) < dbx and abs(point1[1]-point2[1]) < dby

Center = lambda centerf, visiblef, db=15, px=0.001, py=0.001, dx=0.00005, dy=0.00005: Sequential(
            Log('Centering'),
            MasterConcurrent(
                Consistent(lambda: close_to(centerf(), CAM_CENTER,  db), count=2.5, total=3.0, invert=False, result=True),
                Consistent(visiblef, count=2.5, total=3.0, invert=True, result=False),
                While(lambda: DownwardTarget(point=centerf, target=CAM_CENTER, deadband=(0, 0), px=px, py=py), True),
                AlwaysLog(lambda: 'center = {}, target = {}'.format(centerf(), CAM_CENTER))))

CenterAny = lambda: Center(center_any, any_visible)

# Descend = lambda depth=DEPTH, db=0.1, size_thresh=SIZE_THRESH: Sequential(  # TODO: FIND THE ACTUAL DEPTH1!!
#             Log('Descent into Madness'),
#             MasterConcurrent(  # TODO: TIMEOUT
#                 Consistent(lambda: abs(shm.kalman.depth.get() - depth) < db or size() > size_thresh, count=2.3, total=3, invert=False, result=True),
#                 Depth(depth)),  # TODO: BigDepth?
#             Zero())

close_to = lambda point1, point2, db=20: abs(point1[0]-point2[0]) < db and abs(point1[1]-point2[1]) < db

Align = lambda centerf, anglef, visiblef, closedb=10, aligndb=7: Sequential(
            Log('Aligning'),
            MasterConcurrent(
                Consistent(lambda: close_to(centerf(), CAM_CENTER) and abs(anglef()) < aligndb, count=2.3, total=3, invert=False, result=True),
                While(lambda: Consistent(visiblef, count=2.5, total=3.0, invert=True, result=False), True),
                While(lambda: Center(centerf, visiblef), True),
                PIDHeading(anglef, p=0.47),
                AlwaysLog(lambda: 'align %s' % str(anglef()))),
            Zero())

_Grab = lambda: SetActuators(on_triggers=['manipulator_grab'])
_Release = lambda: Sequential(
                    SetActuators(on_triggers=['manipulator_release'], off_triggers=['manipulator_grab']),
                    Timer(0.3),
                    SetActuators(off_triggers=['manipulator_release']))

GrabVampireOpenCoffin = lambda: \
    Sequential(
        Search(visible_open),
        Center(center_open, visible_open, db=20),
        SetMarker('before_grab'),
        Align(centerf=center_open, anglef=angle_offset_open, visiblef=visible_open),
        Center(offset_open, visible_open, db=10),
        MasterConcurrent(
            Sequential(
                Timer(15),
                _Grab()),
            RelativeToCurrentDepth(DESCEND_DEPTH, error=0.2),
            ),
        Depth(SEARCH_DEPTH),
        GoToMarker('before_grab'),
        UnsetMarker('before_grab'),
        Timeout(Consistent(visible_open, count=1.5, total=2.0, invert=False, result=True), 10),
        # Grab(),  # ???
        # Release???
    )

LID_DEPTH = 0.4
LID_DEPTH_1 = 0.5

initial_depth = 3

def record_depth():
    global initial_depth
    initial_depth = shm.kalman.depth.get()

GrabVampireClosedCoffin = lambda: \
    Sequential(
        Search(visible_closed),
        Center(center_closed, visible_closed),
        SetMarker('before_grab'),
        Align(center_closed, angle_offset_closed, visible_closed),
        Center(offset_closed, visible_closed, db=10),
        MasterConcurrent(
            Sequential(
                Timer(15),
                _Grab()),
            RelativeToCurrentDepth(DESCEND_DEPTH, error=0.2),
            ),
        RelativeToInitialDepth(-LID_DEPTH_1, error=0.25),
        Log('what'),
        Conditional(Yike(), on_fail=Fail(_Release())),
        GrabVampireOpenCoffin()
    )

Yike = lambda: \
    Sequential(
        MasterConcurrent(
            Sequential(Timed(RelativeToCurrentDepth(-LID_DEPTH), 3.5), RelativeToCurrentDepth(0)),
            VelocityY(0.2 * direction_closed())
        ),
        Timed(VelocityY(0.3), 3),
        Depth(SEARCH_DEPTH, error=0.2),
        GoToMarker('before_grab'),
        Timeout(Consistent(visible_open, count=1.5, total=2.0, invert=False, result=True), 10),
        Log('Opened Coffin Successfully'),
        UnsetMarker('before_grab'),
        )

grabbing = False

def get_grabbing():
    global grabbing
    return grabbing

def set_grabbing(value):
    global grabbing
    grabbing = value
    return Succeed()

GrabVampire = lambda: \
    MasterConcurrent(
        Conditional(
                DualConsistency(visible_open, visible_closed, count=1.5, total=2.0, invert=False),
                on_success=Sequential(set_grabbing(True), GrabVampireOpenCoffin(), set_grabbing(False)),
                on_fail=Sequential(set_grabbing(True), GrabVampireClosedCoffin(), set_grabbing(False))
        ),
        Fail(CheckedTimer(30, get_grabbing, False)),
    )

ReleaseVampire = lambda edge: \
    Sequential(
        MoveNE(edge),
        Depth(0),
        _Release(),
        Timer(2),
        Depth(SEARCH_DEPTH)
    )


ReleaseCrucifix = lambda: \
    Sequential(
        Search(visible_empty),
        Depth(DESCEND_DEPTH),
        Center(center_empty, visible_empty),
        _Release(),
    )


MarkerTest = lambda: \
    Sequential(
        SetMarker('test'),
        Timer(10),
        SetMarker('test2', (0,0)),
        GoToMarker('test'),
        GoToMarker('test2'),
        UnsetMarker('test'),
        GoToMarker('test'))

PosTest = lambda: AlwaysLog(lambda: '{}, {}'.format(shm.kalman.north.get(), shm.kalman.east.get()))
