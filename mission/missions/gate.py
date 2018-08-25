from conf.vehicle import VEHICLE

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, While
from mission.framework.movement import RelativeToInitialHeading, Depth, VelocityX, VelocityY
from mission.framework.position import MoveX
from mission.framework.primitive import Log
from mission.framework.targeting import PIDLoop
from mission.framework.timing import Timed
from mission.framework.task import Task
from mission.framework.helpers import ConsistencyCheck, call_if_function

from .ozer_common import ConsistentTask

from mission.missions.will_common import BigDepth, is_mainsub

from mission.constants.config import gate as settings

import shm

results_groups = shm.bicolor_gate_vision

class Consistent(Task):
    def on_first_run(self, test, count, total, invert, result):
        # Multiple by 60 to specify in seconds
        self.checker = ConsistencyCheck(count * 60, total * 60, default=False)

    def on_run(self, test, count, total, invert, result):
        test_result = call_if_function(test)
        if self.checker.check(not test_result if invert else test_result):
            self.finish(success=result)

XTarget = lambda x, db: PIDLoop(input_value=x, target=0,
                                output_function=VelocityY(), negate=True,
                                p=0.4 if is_mainsub() else 0.4, deadband=db)

DEPTH_TARGET = settings.depth

#gate = Sequential(target, Log("Targetted"), center, Log("Centered"), charge)

# This is the unholy cross between my (Will's) and Zander's styles of mission-writing
gate = Sequential(
    Log('Depthing...'),
    BigDepth(DEPTH_TARGET),
    Log('Lining up...'),
    ConsistentTask(Concurrent(
        Depth(DEPTH_TARGET),
        XTarget(x=results_groups.gate_center_x.get, db=0.03),
        finite=False
    )),
    Log('Driving forward...'),
    MasterConcurrent(
        Consistent(test=lambda: results_groups.width.get() < settings.gate_width_threshold, count=0.2, total=0.3, invert=True, result=True),
        Depth(DEPTH_TARGET),
        VelocityX(0.1 if is_mainsub() else 0.1),
        While(task_func=lambda: XTarget(x=results_groups.gate_center_x.get, db=0.018), condition=True),
    ),
    # Jank
    Timed(VelocityX(0 if is_mainsub() else -0.1), 2),
    VelocityX(0),
    Log('Lining up with red side...'),
    ConsistentTask(Concurrent(
        Depth(DEPTH_TARGET),
        XTarget(x=results_groups.gate_center_x.get, db=0.05),
        finite=False,
    )),
    Log('Charging...'),
    Timed(VelocityX(0.5 if is_mainsub() else 0.2), settings.charge_dist),
    Log('Through gate!'),
)
