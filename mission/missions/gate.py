from mission.framework.combinators import Sequential, Concurrent
from mission.framework.movement import RelativeToInitialHeading, Depth, VelocityX, VelocityY
from mission.framework.position import MoveX
from mission.framework.primitive import Log
from mission.framework.targeting import PIDLoop
from mission.framework.timing import Timed
from mission.framework.task import Task

from .ozer_common import ConsistentTask

import shm

results_groups = shm.bicolor_gate_vision


XTarget = lambda: PIDLoop(input_value=results_groups.red_center_x.get, target=0,
                          output_function=VelocityY(), negate=True, p=1.25, deadband=0.01875)


def get_width():
    width = 2 * abs(results_groups.red_center_x.get() - results_groups.black_center_x.get())
    print("Width: {}".format(width))
    return width

WidthTarget = lambda width: PIDLoop(input_value=get_width, target=width,
                              output_function=VelocityX(), negate=False, p=1.0, deadband=0.01875)

target = ConsistentTask(Concurrent(Depth(1.5), XTarget(), finite=False))
center = ConsistentTask(Concurrent(Depth(1.5), XTarget(), WidthTarget(.8), finite=False))
charge = Timed(VelocityX(.3), 20)


gate = Sequential(target, Log("Targetted"), center, Log("Centered"), charge)
