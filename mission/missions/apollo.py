'''
     _______	_	  _	     _________		   __	     _     _  __   		  __
    /  ______\ | \	  / |    /  _    _ \		  /  \	    | \	  / | \ \ 		 / /
    | |            | |	 | |   /  | \__  \ \     / /\ \	    | |	  | |  \ \      / /
    | |            | |	 | |  | / |    \  | |   / /  \ \    | |	  | |   \ \    / /
    | |            | |	  | |  | |  \__  | / |  / /	  \ \   | |	  | |	 \ \  / /
    | |______  | |___| |   \ \_	  \_|  /  / /  	   \ \  | |___| |	  \ \/ /
    \_______/  \_______/    \_________/	 /_/		\_\ \_______/  	   \__/

	   ___   			 __ __
 	  / , |  ___  ___   / // /___
	 / _  | / _ \/ _ \ / // // _ \
	/_/ |_|/ ___/\___//_//_/ \___/
		  /_/
   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  /_  /
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / / /
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/ /_/

'''

import os
import shm
import datetime

from mission.missions.master_common import *
from mission.opt_aux.aux import *
from mission.framework.combinators import Sequential, Retry
from mission.missions.gate import ApolloGate as Gate
from mission.missions.buoys import AllBuoys as Buoys
from mission.missions.buoys import RetryBuoys
from mission.missions.wire import full as Wire
from mission.missions.pipe import OptimizablePipe as Pipe
from mission.missions.hydrophones import OptimizablePinger as FindPinger
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading, Zero
from mission.constants.region import *
from sensors.kalman.set_zero_heading import set_zero_heading

Gate = MissionTask(
	name = 'Gate',
	cls = Gate,
	modules = None,
	surfaces= False
)

PipeToBuoys = MissionTask(
	name = 'PipeToBuoys',
  	cls = lambda: Pipe(grp = shm.buoys_pipe_results),
  	modules = [shm.vision_modules.Pipes],
  	surfaces = False
)

Buoys = MissionTask(
  	name = 'Buoys',
  	cls = Buoys,
  	modules = [shm.vision_modules.Buoys],
  	surfaces = False
)

PipeToWire = MissionTask(
  	name = 'PipeToWire',
  	cls = lambda: Pipe(grp = shm.wire_pipe_results),
  	modules = [shm.vision_modules.Pipes],
  	surfaces = False
)

Wire = MissionTask(
  	name = 'Wire',
  	cls = Wire,
  	modules = [shm.vision_modules.Wire],
  	surfaces = False
)

RetryBuoys = MissionTask(
    name = 'RetryBuoys',
    cls = RetryBuoys,
    modules = [shm.vision_modules.Buoys],
    surfaces = False
)


tasks = [
	Gate,
	PipeToBuoys,
	Buoys,
	PipeToWire,
	Wire,
      RetryBuoys
]

Full = RunAll(tasks)
