'''
Guard against behaviors which are silly.
'''

from mission.framework.task import *
from mission.framework.movement import *
from mission.framework.position import *

from enum import Enum
from conf.auv_locale import bounds as locale_bounds

import aslam
import numpy as n

class GuardState(Enum):
  RUNNING   = 0
  GUARDING  = 1

class Guarded(Task):
  def on_first_run(self, *args, **kwargs):
    self.state = GuardState.RUNNING

  def on_run(self, condition, description, desired_task, restore_cls, *args, **kwargs):
    if self.state is GuardState.RUNNING:
      ok = condition()   
      if ok:
        desired_task()
        if desired_task.finished:
          self.finish()
      else:
        self.logw('Guarded task running with condition "{}" was triggered, swapping to restore task'.format(description))
        self.state = GuardState.GUARDING
        self.restore_instance = restore_cls()

    elif self.state is GuardState.GUARDING:
      self.restore_instance()
      if self.restore_instance.finished:
        self.logi('Restore task for guarded task running with condition "{}" finished, swapping back to main task'.format(description))
        self.state = GuardState.RUNNING

def within(vec, bounds):
  return all(bounds[ind][0] <= vec[ind] <= bounds[ind][1] for ind in range(len(bounds)))

def center(bounds):
  return n.array([(bound[0] + bound[1]) / 2. for bound in bounds])

def unitize(vec):
  return vec / n.linalg.norm(vec)

def TowardsCenter():
  ctr = center(locale_bounds)
  now = aslam.sub.position()
  direc = unitize(ctr - now)
  dest  = direc * 1.
  return GoToPosition(dest[0], dest[1], depth = dest[2])

LocaleBounded = lambda task: Guarded(
  lambda: within(aslam.sub.position(), locale_bounds),
  'avoid locale bound escapement',
  task,
  TowardsCenter
)
