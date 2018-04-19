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
from mission.missions.opt import Opt, assertModules, killAllModules
from mission.missions.start import WaitForUnkill
from mission.framework.movement import *
from mission.framework.position import *
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, HardkillGuarded, \
                                        EnableController, ZeroWithoutHeading, Zero, Log
from mission.constants.region import *
from sensors.kalman.set_zero_heading import set_zero_heading
from collections import namedtuple

class Begin(Task):
  def on_first_run(self, *args, **kwargs):
    self.killed = shm.switches.hard_kill.get()

    self.use_task(Sequential(
      Log('Waiting for Hard Kill'),
      WaitForUnkill(killed=False),
      Log('Waiting for unkill!'),
      FunctionTask(lambda: shm.switches.soft_kill.set(1)),
      FunctionTask(lambda: shm.deadman_settings.enabled.set(False)),
      WaitForUnkill(),
      #Log('Setting zero heading'),
      #FunctionTask(set_zero_heading),
      Log('Zeroing'),
      Zero(),
      FunctionTask(lambda: shm.switches.soft_kill.set(0)),
      EnableController(),
    ))
