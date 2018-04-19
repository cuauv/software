'''
     _______	_	  _	     _________		   __	     _     _  __   		  __
	/  _____\  | \	 / |    /  _    _ \		  /  \	    | \	  / | \ \ 		 / /
	| |		   | |	 | |   /  | \__  \ \     / /\ \	    | |	  | |  \ \      / /
	| |		   | |	 | |  | / |    \  | |   / /  \ \    | |	  | |   \ \    / /
	| |		   | |	 | |  | |  \__  | / |  / /	  \ \   | |	  | |	 \ \  / /
	| |______  | |___| |   \ \_	  \_|  /  / /  	   \ \  | |___| |	  \ \/ /
	\_______/  \_______/    \_________/	 /_/		\_\ \_______/  	   \__/

	   ___   		__
 	  / , |  ____ _/ /_ ___   __  __   __ ___
	 / _  | / __//_  _// . ) /  |/  | / /(_-<
	/_/ |_|/_/    /_/  \__\ /_/|_/|_|/_//___/

   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  /_  /
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / / /
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/ /_/

'''

import numpy as np

import shm

from mission.constants.region import TOWARDS_BOWL_SIGN
from mission.constants.config import recovery as recovery_constants

from mission.framework.task import Task
from mission.framework.primitive import Zero, Log, FunctionTask
from mission.framework.movement import RelativeToCurrentDepth, RelativeToCurrentHeading, \
    Pitch, Depth, Heading
from mission.framework.position import NavigationSpeed, MoveX
from mission.framework.combinators import Sequential, MasterConcurrent
from mission.missions.ozer_common import WithQuaternionControl, ConsistentTask

from mission.missions.master_common import RunAll, MissionTask, HydrophonesWithVision
from mission.missions.gate import ArtemisGate as Gate
from mission.missions.recovery import Full as Recovery, Found as FoundRecovery
from mission.missions.torpedoes import Full as Torpedoes
from mission.missions.bins import Full as Bins, Found as FoundBins
from mission.missions.balls import Full as Balls

class HydrophonesDeadReckon(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(NavigationSpeed(Sequential(
            Depth(recovery_constants.tower_depth, error=0.08),
            ConsistentTask(Heading(60 * TOWARDS_BOWL_SIGN)),
            MoveX(30, deadband=0.08),
        ), 0.4))

class PrintDistance(Task):
    def on_first_run(self, *args, **kwargs):
        k = shm.kalman.get()
        self.init_pos = np.array([k.north, k.east])

    def on_run(self, *args, **kwargs):
        k = shm.kalman.get()
        pos = np.array([k.north, k.east])
        dist = np.linalg.norm(pos - self.init_pos)
        self.logi('Distance: {}'.format(dist))

class Celebrate(Task):

    class CheckDepth(Task):
        def on_run(self, *args, **kwargs):
            if shm.kalman.depth.get() < 0.4:
                self.finish()

    def on_first_run(self, *args, **kwargs):
        self.use_task(WithQuaternionControl(Sequential(
            Log('Time to celebrate!'),

            Depth(3, error=0.2),
            Pitch(90, error=5),

            Log('Weeeeeeeeeee'),
            MasterConcurrent(
                self.CheckDepth(),
                RelativeToCurrentDepth(-0.25),
                RelativeToCurrentHeading(10),
            ),
            FunctionTask(lambda: shm.settings_control.enabled.set(False)),
            Zero(),
        )))

gate = MissionTask(
	name = 'Gate',
	cls = Gate,
	modules = None,
	surfaces= False,
)

recovery = MissionTask(
	name = 'Recovery',
	cls = Recovery,
	modules = [shm.vision_modules.Recovery],
	surfaces = True,
)

recovery_pinger = MissionTask(
	name = 'RecoveryPinger',
    cls = lambda: HydrophonesWithVision(FoundRecovery()),
	modules = [shm.vision_modules.Recovery],
	surfaces = False,
)

bins_pinger = MissionTask(
	name = 'RecoveryPinger',
    cls = lambda: HydrophonesWithVision(FoundBins()),
	modules = [shm.vision_modules.Bins],
	surfaces = False,
)

torpedoes = MissionTask(
	name = 'Torpedoes',
    cls = Torpedoes,
	modules = [shm.vision_modules.Torpedoes],
	surfaces = False,
)

bins = MissionTask(
	name = 'Bins',
    cls = Bins,
	modules = [shm.vision_modules.Bins],
	surfaces = False,
)

hydrophones_dead_reckon = MissionTask(
	name = 'HydrophonesDeadReckon',
    cls = HydrophonesDeadReckon,
	modules = [],
	surfaces = False,
)

balls = MissionTask(
    name = 'Balls',
    cls = Balls,
    modules = [],
    surfaces = True,
)

celebrate = MissionTask(
    name = 'Celebrate',
    cls = Celebrate,
    modules = [],
    surfaces = True,
)

# Good recovery pinger option
tasks = [
    gate,
    hydrophones_dead_reckon,
    # recovery_pinger,
    recovery,
    # bins_pinger,
    bins,
    torpedoes,

    # recovery_pinger,
    # balls,
    # celebrate,
]

def Full(): return RunAll(tasks)
