'''
   _______    _     _      _________         __        _     _  __          __
  /  _____\  | \   / |    /  _    _ \       /  \      | \   / | \ \        / /
  | |        | |   | |   /  | \__  \ \     / /\ \     | |   | |  \ \      / /
  | |        | |   | |  | / |    \  | |   / /  \ \    | |   | |   \ \    / /
  | |        | |   | |  | |  \__  | / |  / /    \ \   | |   | |    \ \  / /
  | |______  | |___| |   \ \_   \_|  /  / /      \ \  | |___| |     \ \/ /
  \_______/  \_______/    \_________/  /_/        \_\ \_______/      \__/


   ___       __                 __     ___  ___ _______
  / _ \___  / /  ___  ___ __ __/ /    |_  |/ _ <  /_  /
 / , _/ _ \/ _ \/ _ \(_-</ // / _ \  / __// // / / / /
/_/|_|\___/_.__/\___/___/\_,_/_.__/ /____/\___/_/ /_/

'''

import os
import shm
import datetime
import traceback

from mission.framework.task import Task
from mission.opt_aux.aux import *
from mission.framework.combinators import Sequential, Retry, Concurrent
from mission.missions.opt import Opt, assertModules, killAllModules
from mission.missions.start import WaitForUnkill
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading, Zero, Succeed, \
                                        Log, Fail
from mission.constants.region import *
from sensors.kalman.set_zero_heading import set_zero_heading
from mission.missions.hydrophones import Full as Hydrophones
from collections import namedtuple

class RunTask(Task):
  def on_first_run(self, task, *args, **kwargs):
    self.task = task
    self.taskCls = task.cls()
    if not callable(self.taskCls):
      self.taskCls = task.cls
    self.exceptionCount = 0
    self.maxExceptionCount = 3 #TODO: make settable or more logical


    self.logi('Starting {} task!'.format(task.name))

  def on_run(self, *args, **kwargs):
    #Block non-surfacing tasks from surfacing
    if not self.task.surfaces and (shm.desires.depth.get() < 0.3 or shm.kalman.depth.get() < .3):
      Depth(0.3)()
      self.logw('Task attempted to rise above 0.3!')

    #start only required modules
    assertModules(self.task.modules, self.logi)

    #actually run the bloody mission
    try:
      self.taskCls()
    except Exception as e:
      self.exceptionCount += 1
      if self.exceptionCount < self.maxExceptionCount:
        self.logw('Task {} threw exception: {}! Exception {} of {} before that task is killed!'.format(self.task.name, \
          e, self.exceptionCount, self.maxExceptionCount))
        traceback.print_exc()
      else:
        self.loge('Task {} threw exception: {}! Task has reached exception threshold, will no longer be attempted!'.format( \
          self.task.name, e))
        self.finish()
    if self.taskCls.finished:
      if self.task.name == 'EndMission':
        self.finish(success=False)
      else:
        self.finish()

  def on_finish(self):
    #self.logi("Task {} was finished!".format(self.task.name))
    self.logv('{} task finished in {} seconds!'.format(
            self.task.name,
            self.this_run_time - self.first_run_time))
    Zero()()
    killAllModules(self.logi)
    EnableController()()
    shm.settings_control.quat_pid.set(False)

MissionTask = namedtuple('MissionTask', [
  'name',
  'cls',
  'modules',
  'surfaces',
  ])

class RunAll(Task):
  def on_first_run(self, tasks, *args, **kwargs):
    #tasks.insert(0, BeginMission)
    tasks.append(EndMission)

    self.use_task(Retry(lambda: Sequential(
      RunTask(BeginMission),
      Concurrent(
        Fail(Sequential(subtasks=[RunTask(t) for t in tasks],)),
        Fail(WaitForUnkill(killed=False, wait=1)),

      ),
      ), float('inf'))
    )

class Begin(Task):
  def on_first_run(self, *args, **kwargs):
    self.killed = shm.switches.hard_kill.get()

    self.use_task(Sequential(
      FunctionTask(lambda: shm.switches.soft_kill.set(1)),
      FunctionTask(lambda: shm.deadman_settings.enabled.set(False)),
      Log('Disabling Record vision module'),
      FunctionTask(lambda: shm.vision_modules.Record.set(0)),
      WaitForUnkill(killed=False, wait=.5),
      WaitForUnkill(),
      Log('Zeroing'),
      Zero(),
      FunctionTask(lambda: shm.switches.soft_kill.set(0)),
      EnableController(),
      Heading(0),
      Log('Enabling Record vision module'),
      FunctionTask(lambda: shm.vision_modules.Record.set(1)),
    ))


class End(Task):
  def on_first_run(self, *args, **kwargs):
    self.use_task(Sequential(
      Log('Ending Run! Surfacing and softkilling'),
      Zero(),
      Depth(0),
      FunctionTask(lambda: shm.switches.soft_kill.set(1)),
      FunctionTask(lambda: shm.deadman_settings.enabled.set(True)),
    ))

class HydrophonesWithVision(Task):
  # TODO use a ConcurrentOr combinator instead?
  def on_first_run(self, vision, *args, **kwargs):
    self.hydro = Hydrophones()
    self.vision = vision

  def on_run(self, *args, **kwargs):
    self.hydro()
    self.vision()
    if self.hydro.finished:
      self.logi('Hydrophones finished without seeing anything')
      self.finish()
    elif self.vision.finished:
      self.logi('Hydrophones finished after seeing mission element in vision')
      self.finish()

BeginMission = MissionTask(
  name = 'BeginMission', #DON'T CHANGE THIS!!!!
  cls = Begin,
  modules = None,
  surfaces = True
)

EndMission = MissionTask(
  name = 'EndMission', #DON'T CHANGE THIS!!!!
  cls = End,
  modules = None,
  surfaces = True
)
