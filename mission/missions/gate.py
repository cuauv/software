from mission.framework.combinators import Sequential, Concurrent
from mission.framework.movement import RelativeToInitialHeading, Depth, VelocityY
from mission.framework.position import MoveX
from mission.framework.primitive import Log
from mission.framework.targeting import PIDLoop
from mission.framework.task import Task

import shm

results_groups = shm.yellow_buoy_results


class XTarget(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop_x = PIDLoop(output_function=VelocityY(), negate=True)

    def on_run(self, *args, **kwargs):
        point = results_groups.center_x.get()
        self.pid_loop_x(input_value=point, target=0, p=0.8, deadband=0.01875)


target = Concurrent(Depth(1), XTarget())
