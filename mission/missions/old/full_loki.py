'''
   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  / __/
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / _ \
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/\___/

'''

import shm
import aslam

from mission.opt_aux.aux import *
from mission.missions.opt import Opt
from mission.framework.task import Task

# Guessed on the imports, please update for your task.

from mission.missions.aslam_buoys import AllBuoys as Buoys
from mission.missions.wire import full as Wire
from mission.missions.pipe import OptimizablePipe as Pipe
from mission.missions.guard import LocaleBounded
#from mission.missions.recovery import mission as Recovery

PipeToBuoys = OptimizableTask(
  name = 'PipeToBuoys',
  cls = lambda: Pipe(grp = shm.buoys_pipe_results),
  startPosition = aslam.world.pipe_to_buoys.position
)

Buoys = OptimizableTask(
  name = 'Buoys',
  cls = Buoys,
  startPosition = aslam.world.red_buoy.position
)

PipeToWire = OptimizableTask(
  name = 'PipeToWire',
  cls = lambda: Pipe(grp = shm.wire_pipe_results),
  startPosition = aslam.world.pipe_to_wire.position
)

Wire = OptimizableTask(
  name = 'Wire',
  cls = Wire,
  startPosition = aslam.world.wire.position
)

restrictions = [
  TopologicalRestriction(beforeTask = 'PipeToBuoys', afterTask = 'Buoys'),
  TopologicalRestriction(beforeTask = 'Buoys', afterTask = 'PipeToWire'),
  TopologicalRestriction(beforeTask = 'PipeToWire', afterTask = 'Wire'),
]

tasks = [
  PipeToBuoys,
  Buoys,
  PipeToWire,
  Wire,
]

Full = LocaleBounded(
  Opt(tasks = tasks, restrictions = restrictions)
)

for task in tasks:
  globals()[task.name] = Opt(tasks = [task], restrictions = [])
