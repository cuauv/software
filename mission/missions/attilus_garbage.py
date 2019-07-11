from mission.framework.task import Task
from mission.framework.helpers import ConsistencyCheck, call_if_function
from mission.framework.targeting import PIDLoop
from mission.framework.movement import VelocityX, VelocityY, RelativeToCurrentHeading, RelativeToInitialHeading
from mission.framework.position import MoveX
from mission.framework.combinators import While, Sequential, MasterConcurrent
from mission.framework.primitive import FunctionTask, Succeed

import shm
"""
A bunch of garbage that I (Attilus) want to use across different missions.
"""

# A task that runs a PID loop for VelocityY
class PIDSway(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=VelocityY())

    def on_run(self, error, p=0.0005,  i=0, d=0.0, db=0.01875, max_out=0.5, negate=False, *args, **kwargs):
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=False, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        VelocityY(0)()

# A task that runs a PID loop for VelocityX
class PIDStride(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=VelocityX())

    def on_run(self, error, p=0.00003,  i=0, d=0.0, db=0.01875, negate=False, max_out=0.5, *args, **kwargs):
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=False, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        VelocityY(0)()

# A task that runs a PID loop for Heading
class PIDHeading(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=RelativeToCurrentHeading())

    def on_run(self, error, p=0.35,  i=0, d=0.0, db=0.01875, negate=False, max_out=20, *args, **kwargs):  # TODO: max_out
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=360, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        RelativeToCurrentHeading(0)()

class StillHeadingSearch(Task):
    """
    Search for an object visible from the current location that is in front of
    the sub with highest probability. Edited from ozer_common
    """

    def on_first_run(self, speed=20, db=10, *args, **kwargs):
        init_heading = None

        def set_init_heading():
            nonlocal init_heading
            init_heading = shm.kalman.heading.get()
            return True

        set_init_heading()

        self.use_task(
            While(lambda: Sequential(
                RelativeToInitialHeading(speed),
                MasterConcurrent(
                    FunctionTask(lambda: abs(shm.desires.heading.get() - init_heading) < db, finite=False),
                    RelativeToCurrentHeading(speed)),
                # Move back a bit, we might be too close
                MoveX(-1),
                Succeed(FunctionTask(set_init_heading))
            ), True),
        )


