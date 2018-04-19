from collections import namedtuple

from mission.framework.helpers import *
from mission.framework.movement import *
from mission.framework.task import *

import aslam, shm, time, math, random, enum

# Type 1: Object and callable position (likely relative to object).
# (object is needed so that Trajectory can turn on/off relevant sensors).

CallableObjectPosition = namedtuple('CallableObjectPosition', [
  'name',
  'position',
  'object',
  'tolerance'
])

# Type 2: Callable position only.

CallablePosition = namedtuple('CallablePosition', [
  'name',
  'position',
  'tolerance'
])

# Type 2: Callable orientation. Used to e.g. turn the sub towards the buoy.
 
CallableOrientation = namedtuple('CallableOrientation', [
  'name',
  'orientation',
  'tolerance'
])

# Type 3: Relative callable position. Used to e.g. move two meters forward and ram the buoy.
# Relative is given as a lambda which returns (dx, dy, depth).

RelativePosition = namedtuple('RelativePosition', [
  'name',
  'relative',
  'tolerance'
])

# Type 4: Action - callable task. Will be run until finished at that point in the trajectory.
FiniteTask = namedtuple('FiniteTask', [
  'name',
  'task'
])

# Type 5: Location mark. Location should be an identifier string (can be used later in ReturnToLocation).

MarkLocation = namedtuple('MarkLocation', [
  'name',
  'location'
])

# Type 6: Location return. Location should be an identifier string and *must* have been marked before.

ReturnToLocation = namedtuple('ReturnToLocation', [
  'name',
  'location'
])

class Trajectory(Task):
  '''
  Generic trajectory execution task.

  :param constraints    : Trajectory constraints
  :param log_verbosely  : Dump lots of logs to stdout
  '''

  def on_first_run(self, constraints, log_verbosely = False):
    pass

  def on_run(self, constraints, log_verbosely = False):
    pass
