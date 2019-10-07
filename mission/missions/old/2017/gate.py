from mission.framework.combinators import Sequential
from mission.framework.movement import Depth, VelocityX
from mission.framework.position import MoveXRough
from mission.framework.primitive import Log, Zero
from mission.framework.timing import Timed
from mission.framework.task import Task
from mission.constants.config import gate as constants

class ArtemisGate(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(Sequential(
            Log('Going to gate depth'),
            Depth(constants.depth),
            Log('Moving through gate'),
            Timed(VelocityX(0.4), constants.artemis_vel_time),
            Zero(),
        ))

class ApolloGate(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(Sequential(
            Log('Going to gate depth'),
            Depth(constants.depth),
            Log('Moving through gate'),
            Timed(VelocityX(0.4), constants.vel_time),
            Zero(),
        ))
