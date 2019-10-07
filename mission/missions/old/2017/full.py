'''
   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  /_  /
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / / /
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/ /_/

'''

import os
import shm
import datetime

from mission.opt_aux.aux import *
from mission.framework.combinators import Sequential
from mission.missions.opt import Opt
from mission.missions.gate import gate
from mission.missions.buoys import AllBuoys as Buoys
from mission.missions.wire import full as Wire
from mission.missions.pipe import OptimizablePipe as Pipe
from mission.missions.hydrophones import OptimizablePinger as FindPinger
from mission.missions.start import WaitForUnkill
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading, Zero
from mission.constants.region import *
from sensors.kalman.set_zero_heading import set_zero_heading

PipeToBuoys = OptimizableTask(
  name = 'PipeToBuoys',
  cls = lambda: Pipe(grp = shm.buoys_pipe_results),
  maxTries = 1,
  noProgressKillTime = 60
)

Buoys = OptimizableTask(
  name = 'Buoys',
  cls = Buoys,
  maxTries = 1,
  noProgressKillTime = 120
)

PipeToWire = OptimizableTask(
  name = 'PipeToWire',
  cls = lambda: Pipe(grp = shm.wire_pipe_results),
  maxTries = 1,
  noProgressKillTime = 60
)

Wire = OptimizableTask(
  name = 'Wire',
  cls = Wire,
  maxTries = 1,
  noProgressKillTime = 90
)

Pinger = OptimizableTask(
  name = 'FindPinger',
  cls = FindPinger,
  maxTries = 3,
  noProgressKillTime = 120
)

restrictions = [
]

tasks = [
  PipeToBuoys,
  Buoys,
  PipeToWire,
  Wire,
  #Pinger
]

after_unkill = lambda task: HardkillGuarded(Sequential(
  # Because WaitForUnhardKill should grab heading when sub switched on.
  #ZeroWithoutHeading(),
  Zero(),
  #FunctionTask(lambda: set_zero_heading()),
  task,
  #Depth(0.0)
))

Run = lambda task: Sequential(
  EnableController(),
  WaitForUnkill(),
  after_unkill(task)
)

Full = Run(Sequential(
  # gate,
  Opt(tasks = tasks, restrictions = restrictions),
))

defaultOrdering = tasks

killed = shm.switches.hard_kill.get()

if killed:
  print('HARD KILLED - mission will WAIT for unkill!')
else:
  print('NOT hard killed, running mission!')

def make(task):
  if killed:
    return Sequential(WaitForUnkill(), task)
  else:
    return task

for task in tasks:
  globals()[task.name] = make(Opt(tasks = [task], restrictions = []))

  globals()['After' + task.name] = make(Opt(
    tasks = defaultOrdering[defaultOrdering.index(task) + 1:],
    restrictions = restrictions,
    alreadyFinishedTasks = [task.name for task in defaultOrdering[:defaultOrdering.index(task) + 1]]
  ))
