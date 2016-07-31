'''
   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  / __/
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / _ \
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/\___/

'''

import os
import shm
import datetime

from mission.opt_aux.aux import *
from mission.framework.combinators import Sequential
from mission.missions.opt import Opt
from mission.missions.gate import gate
from mission.missions.aslam_buoys import AllBuoys as Buoys
from mission.missions.navigate import full as Navigation
from mission.missions.pipe import OptimizablePipe as Pipe
from mission.missions.random_pinger import RandomPinger
from mission.missions.start import WaitForUnkill
from mission.missions.recovery import OptimalRecovery as Recovery
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.missions.ozer_common import Timeout
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading
from mission.constants.region import *
from mission.missions.random_pinger import NONE, RECOVERY


PipeToBuoys = OptimizableTask(
  name = 'PipeToBuoys',
  cls = lambda: Pipe(grp = shm.buoys_pipe_results),
  maxTries = 1,
  noProgressKillTime = 60
)

# TODO FIXME go to pipe pos FIRST

class Buoys(Task):
  def desiredModules(self):
    return []

  def on_first_run(self):
    self.has_made_progress = False
    self.subtask = Sequential(
      Depth(OVER_BUOYS_DEPTH),
      MoveX(OVER_BUOYS_DISTANCE)
      )

  def on_run(self):
    self.subtask()
    if self.subtask.finished:
      self.finish()

Buoys = OptimizableTask(
  name = 'Buoys',
  cls = Buoys,
  maxTries = 1,
  noProgressKillTime = 120
)

PipeToNavigation = OptimizableTask(
  name = 'PipeToNavigation',
  cls = lambda: Pipe(grp = shm.navigate_pipe_results),
  maxTries = 1,
  noProgressKillTime = 60
)

Navigation = OptimizableTask(
  name = 'Navigation',
  cls = Navigation,
  maxTries = 1,
  noProgressKillTime = 90
)

FirstPinger = OptimizableTask(
  name = 'FirstPinger',
  cls = RandomPinger,
  maxTries = 3,
  noProgressKillTime = 120
)

SecondPinger = OptimizableTask(
  name = 'SecondPinger',
  cls = RandomPinger,
  maxTries = 3,
  noProgressKillTime = 120
)

FirstPinger2 = OptimizableTask(
  name = 'FirstPinger2',
  cls = RandomPinger,
  maxTries = 3,
  noProgressKillTime = 120
)

SecondPinger2 = OptimizableTask(
  name = 'SecondPinger2',
  cls = RandomPinger,
  maxTries = 3,
  noProgressKillTime = 120
)

Recovery = OptimizableTask(
  name = 'Recovery',
  cls = Recovery,
  maxTries = 1,
  noProgressKillTime = 6000
)

restrictions = [
]

reset_random_task   = FunctionTask(lambda: shm.mission_state.random_task.set(NONE))
random_dead_reckon  = FunctionTask(lambda: shm.mission_state.random_task.set(RECOVERY))

tasks = [
  #PipeToBuoys,
  #Buoys,
  #PipeToNavigation,
  #Navigation,
  FirstPinger,
  SecondPinger,
  FirstPinger2,
  SecondPinger2
]

# please don't take this as coding advice for future generations of AUVers
link_stage_path = os.path.join(os.environ['CUAUV_SOFTWARE'], 'link-stage')

Call = lambda name: FunctionTask(lambda: os.system('{}/{}'.format(link_stage_path, name)))

RestartKalman = Call('trogdor restart kalmand')

after_unkill = lambda task: HardkillGuarded(Sequential(
  # Because WaitForUnhardKill should grab heading when sub switched on.
  ZeroWithoutHeading(),
  Call('auv-zero-heading'),
  RestartKalman,
  task,
  Depth(0.0)
))

Run = lambda task: Sequential(
  EnableController(),
  WaitForUnkill(),
  FunctionTask(lambda: shm.vision_modules.Record.set(True)),
  Call('auv-shmlogd -o {}/shmlog-{}.shmlog &'.format(os.environ['CUAUV_SOFTWARE'], datetime.datetime.now().strftime('%Y-%m-%d-%H-%M-%S'))),
  after_unkill(task)
)

FullSwitched = Run(Sequential(
  reset_random_task,
  gate,
  Depth(0.5),
  MoveY(5.0 * TOWARDS_BOWL_SIGN, deadband=0.08),
  MoveX(22.0, deadband=0.08),
  Opt(tasks = tasks, restrictions = restrictions),
  GoToPosition(30.0, -10.0, depth = 0.5, optimize = True),
  Heading(180.0),
  Opt(tasks = [Navigation, PipeToNavigation, Buoys, PipeToBuoys], restrictions = [])
))

DeadReckon = Run(Sequential(
  random_dead_reckon,
  gate,
  Depth(0.5),
  MoveY(5.0 * TOWARDS_BOWL_SIGN, deadband=0.08),
  Timeout(GoToPosition(RECOVERY_NORTH, RECOVERY_EAST, depth = RECOVERY_DEPTH), 60),
  Opt(tasks = [Recovery, SecondPinger], restrictions = [])
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
