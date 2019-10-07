import shm

from mission.framework.search import SearchFor, SwaySearch, SpiralSearch
from mission.framework.combinators import Sequential, MasterConcurrent, While
from mission.framework.primitive import Zero, Log, AlwaysLog, FunctionTask
from mission.framework.targeting import DownwardTarget
from mission.framework.movement import Depth, RelativeToInitialDepth, RelativeToCurrentDepth, VelocityY
from mission.framework.position import MoveY
from mission.framework.timing import Timeout, Timer, Timed
from mission.framework.actuators import FireActuator, SetActuators

from mission.missions.will_common import Consistent
from mission.missions.attilus_garbage import PIDHeading

from mission.missions.vampire import Search, Center, Align, _Grab, INITIAL_DEPTH, DESCEND_DEPTH, SEARCH_DEPTH

CAM_CENTER = shm.recovery_crucifix.cam_x.get(), shm.recovery_crucifix.cam_y.get()


def visible_crucifix():
    return shm.recovery_crucifix.visible.get()
def center_crucifix():
    return (shm.recovery_crucifix.center_x.get(), shm.recovery_crucifix.center_y.get())
def offset_crucifix():
    return (shm.recovery_crucifix.offset_x.get(), shm.recovery_crucifix.offset_y.get())
def angle_offset_crucifix():
    return shm.recovery_crucifix.angle_offset.get()
def size_crucifix():
    return shm.recovery_crucifix.size.get()


GrabCrucifix = lambda: \
    Sequential(
        Search(visible_crucifix),
        Center(center_crucifix, visible_crucifix),
        Align(center_crucifix, angle_offset_crucifix, visible_crucifix),
        Center(offset_crucifix, visible_crucifix, db=10),
        MasterConcurrent(
            Sequential(
                Timer(15),
                _Grab()),
            RelativeToCurrentDepth(DESCEND_DEPTH, error=0.2),
            ),
        Depth(SEARCH_DEPTH, error=0.2),
        Timeout(Consistent(visible_crucifix, count=1.5, total=2.0, invert=True, result=True), 10),
        Log('crucifix grabbed successfully'),
    )

SearchCrucifix = lambda: Search(visible_crucifix)
CenterCrucifix = lambda: Center(center_crucifix, visible_crucifix)
