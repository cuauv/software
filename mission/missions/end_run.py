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
from mission.framework.combinators import Sequential, While
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

class End(Task):
  def on_first_run(self, *args, **kwargs):

    self.use_task(Sequential(
      Log('Ending Run! Surfacing and softkilling'),
      Zero(),
      Depth(0),
      FunctionTask(lambda: shm.switches.soft_kill.set(1)),
      FunctionTask(lambda: shm.deadman_settings.enabled.set(True)),
    ))