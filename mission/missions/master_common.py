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
import subprocess
import threading
import time

from mission.framework.task import Task
from mission.opt_aux.aux import *
from mission.framework.combinators import MasterConcurrent, Sequential, Retry, Concurrent, Conditional, Either
from mission.missions.opt import Opt, assertModules, killAllModules
from mission.missions.start import WaitForUnkill
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading, Zero, Succeed, \
                                        Log, Fail
from mission.constants.region import *

from mission.missions.will_common import Consistent, BigDepth, FakeMoveX

from sensors.kalman.set_zero_heading import set_zero_heading

from mission.missions.leds import AllLeds

from mission.constants.config import NONSURFACE_MIN_DEPTH as MIN_DEPTH

from collections import namedtuple

# from hydrocode.scripts.udp_set_gain import set_gain
# from hydrocode.scripts.udp_set_gain_12 import set_gain as set_gain_12
# from hydrocode.scripts.udp_set_gain_13 import set_gain as set_gain_13


class RunTask(Task):
  def on_first_run(self, task, *args, **kwargs):
    # Kind of hacky, but should let us arbitrarily pick MissionTasks for random pinger
    if callable(task):
      task = task()
    self.task = task
    self.taskCls = task.cls()
    if not callable(self.taskCls):
      self.taskCls = task.cls
    self.exceptionCount = 0
    self.maxExceptionCount = 3 #TODO: make settable or more logical
    self.timeout = task.timeout
    self.on_exit = task.on_exit
    self.on_timeout = task.on_timeout
    self.timed_out = False

    self.logv('Will timeout after {} seconds'.format(self.timeout))


    self.logi('Starting {} task!'.format(task.name))

  def block_surface(self):
    #Block non-surfacing tasks from surfacing
    if not self.task.surfaces and (shm.desires.depth.get() < MIN_DEPTH):   #  or shm.kalman.depth.get() < MIN_DEPTH):
      Depth(max(MIN_DEPTH, shm.desires.depth.get()))()
      self.logw('Task attempted to rise above min depth, {}!'.format(MIN_DEPTH))

  def on_run(self, *args, **kwargs):
    self.block_surface()

    #start only required modules
    assertModules(self.task.modules, self.logi)

    if not self.timed_out and self.timeout is not None and self.this_run_time - self.first_run_time > self.timeout:
      if self.on_timeout is None:
        self.logw('Task timed out! Finishing task...'),
        self.finish()
      else:
        self.logw('Task timed out! Running on_timeout task...'),
        self.timed_out = True

    if not self.timed_out:
      #actually run the bloody mission
      try:
        self.taskCls()
      except Exception as e:
        self.exceptionCount += 1
        if self.exceptionCount < self.maxExceptionCount:
          self.logw('Task {} threw exception: {}! Exception {} of {} before that task is killed!'.format(self.task.name, e, self.exceptionCount, self.maxExceptionCount))
          traceback.print_exc()
        else:
          self.loge('Task {} threw exception: {}! Task has reached exception threshold, will no longer be attempted!'.format(self.task.name, e))
          self.finish()

      if self.taskCls.finished:
        if self.task.name == 'EndMission':
          self.finish(success=False)
        else:
          self.finish()
    else:
      self.on_timeout()

      if self.on_timeout.finished:
        self.finish()

    self.block_surface()

  def on_finish(self):
    if self.on_exit is not None:
      self.logv('Running task on_exit...')
      self.on_exit()

    #self.logi("Task {} was finished!".format(self.task.name))
    self.logv('{} task finished in {} seconds!'.format(
            self.task.name,
            self.this_run_time - self.first_run_time))
    Zero()()
    killAllModules(self.logi)
    EnableController()()
    shm.settings_control.quat_pid.set(False)

# MissionTask = namedtuple('MissionTask', [
#   'name',
#   'cls',
#   'modules',
#   'surfaces',
#   'timeout',
# ])

class MissionTask():
  def __init__(self, name, cls, modules=None, surfaces=False, timeout=None, on_exit=None, on_timeout=None):
    self.name = name
    self.cls = cls
    self.modules = modules
    self.surfaces = surfaces
    self.timeout = timeout
    self.on_exit = on_exit
    self.on_timeout = on_timeout

class RunAll(Task):
  def on_first_run(self, tasks, *args, **kwargs):
    #tasks.insert(0, BeginMission)
    tasks.append(EndMission)

    self.use_task(Retry(lambda: Sequential(
      RunTask(BeginMission),
      Concurrent(
        Sequential(subtasks=[RunTask(t) for t in tasks],),
        Fail(WaitForUnkill(killed=False, wait=1)),
      ),
      ), float('inf'))
    )

class Begin(Task):
  def on_first_run(self, *args, **kwargs):
    self.killed = shm.switches.hard_kill.get()

    self.use_task(Sequential(
      # VisionFramePeriod(0.1), # reset this
      FunctionTask(lambda: shm.switches.soft_kill.set(1)),
      FunctionTask(lambda: shm.deadman_settings.enabled.set(False)),
      Log('Disabling Record vision module'),
      FunctionTask(lambda: shm.vision_modules.Record.set(0)),
      #AllLeds('orange'),

      #Log('Wating for alignment...'),
      #WaitForUnkill(wait=1.0),
      #ZeroHeading(),
      #Log('Aligned heading!'),
      #AllLeds('cyan'),

      # Need a swimmer to do this
      Log('Waiting for re-kill...'),
      WaitForUnkill(killed=False, wait=0.5),
      #AllLeds('blue'),

      Log('Waiting for unkill signal to start mission...'),
      WaitForUnkill(wait=5.0),
      Timer(5),
      Log('Starting mission!'),
      #AllLeds('red'),

      Log('Zeroing'),
      # ZeroHeading(),
      Zero(),
      FunctionTask(lambda: shm.switches.soft_kill.set(0)),
      # EnableController(),
      # Heading(0), # This will revert to the aligned heading
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

# Not used in 2018, perhaps this was 2017?
# class HydrophonesWithVision(Task):
#   # TODO use a ConcurrentOr combinator instead?
#   def on_first_run(self, vision, *args, **kwargs):
#     self.hydro = Hydrophones()
#     self.vision = vision
#
#   def on_run(self, *args, **kwargs):
#     self.hydro()
#     self.vision()
#     if self.hydro.finished:
#       self.logi('Hydrophones finished without seeing anything')
#       self.finish()
#     elif self.vision.finished:
#       self.logi('Hydrophones finished after seeing mission element in vision')
#       self.finish()

# VisionFramePeriod = lambda period: FunctionTask(lambda: shm.vision_module_settings.time_between_frames.set(period))

# def pollux_step():
#   set_gain()
#   time.sleep(30)
#   set_gain_13()
#   time.sleep(30)
#   set_gain_12()
#
# def p_step():
#   threading.Thread(target=pollux_step, daemon=True).start()

# def ConfigureHydromath(enable, gain_high=True):
#   if os.environ["CUAUV_VEHICLE"] == "pollux":
#     return Sequential(
#       FunctionTask(lambda: shm.hydrophones_settings.enabled.set(enable)),
#       FunctionTask(p_step)
#     )
#   else:
#     return Sequential(
#       FunctionTask(lambda: shm.hydrophones_settings.enabled.set(enable)),
#       FunctionTask(set_gain if gain_high else set_gain_12)
#     )


# TrackerGetter = lambda found_roulette, found_cash_in, enable_roulette=True, enable_cash_in=True: Sequential(
#   # Turn on hydromathd
#   ConfigureHydromath(True, enable_cash_in),
#   # Don't kill CPU with vision
#   VisionFramePeriod(track_settings.vision_frame_period),
#   Log('Roulette: ' + str(enable_roulette) + ', Cash-in:' + str(enable_cash_in)),
#   MasterConcurrent(
#     Conditional(
#       # Find either roulette or cash-in
#       Either(
#         Consistent(test=lambda: shm.bins_vision.board_visible.get() if enable_roulette else False,
#                    count=1, total=1.5, invert=False, result=True),
#         Consistent(test=lambda: shm.recovery_vision_downward_bin_red.probability.get() > 0 if enable_cash_in else False,
#                    count=1, total=1.5, invert=False, result=False),
#       ),
#       # Success is roulette
#       on_success=found_roulette,
#       # Failure is cash-in
#       on_fail=found_cash_in,
#     ),
#     # Track with hydrophones
#     Hydrophones(),
#   ),
#   Zero(),
#   # This should end up getting run twice because we call it in on_exit... but just in case
#   TrackerCleanup(),
# )

# TrackerCleanup = lambda: Sequential(
#   # Turn off hydromathd
#   ConfigureHydromath(False, True),
#   # Go back to normal vision settings
#   VisionFramePeriod(0.1), # this should be default
# )

# DriveToSecondPath = Sequential(
#     BigDepth(highway_settings.high_depth),
#     FakeMoveX(dist=highway_settings.dist, speed=highway_settings.speed),
#     BigDepth(highway_settings.low_depth),
# )

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

ZeroHeading = lambda: FunctionTask(set_zero_heading)

# configure_hydrophones = ConfigureHydromath(True, True)

# frick = WaitForUnkill(killed=False, wait=1)
