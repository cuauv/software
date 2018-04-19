from mission.constants.config import PIPE_SEARCH_DEPTH
from mission.framework.combinators import *
from mission.framework.primitive import *
from mission.framework.position import *
from mission.framework.movement import *
from mission.framework.task import *
from mission.framework.timing import *
from mission.framework.wiggling import *
from mission.constants.config import *
from mission.missions.ozer_common import SequentialSuccess, Conditional, Retry
from mission.opt_aux.aux import *
from mission.missions.buoys import Scuttle

import aslam

import shm, time, math
import numpy as n

from auv_math.math_utils import rotate

class GoToPipe(Task):
    """
    Move to and align with the pipe after buoys
    """
    def on_first_run(self, *args, **kwargs):
        pipe_results = shm.buoys_pipe_results.get()
        self.task = Sequential(
            Log('Returning to pipe position'),
            GoToPosition(
                pipe_results.north,
                pipe_results.east,
                depth=pipe_results.depth,
                optimize=True,
            ),
            Log('Aligning with pipe'),
            Heading(pipe_results.heading),
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

class Timeout(Task):
    def on_first_run(self, time, task, *args, **kwargs):
        self.timer = Timer(time)

    def on_run(self, time, task, *args, **kwargs):
        task()
        self.timer()
        if task.finished:
          self.finish()
        elif self.timer.finished:
          self.logw('Task timed out in {} seconds!'.format(time))
          self.finish()

# Will simply override the desired depth set by a task directly after it is called.
class MinDepth(Task):
  def on_run(self, min_depth, subtask):
    actual = shm.kalman.depth.get()
    if actual > min_depth:
      subtask()
    else:
      self.logw('Actual depth {} less than minimum of {}; NOT running task!'.format(actual, min_depth))
    desire = shm.desires.depth.get()
    if desire < min_depth:
      self.logw('Desired depth {} less than minimum of {}; overriding with minimum!'.format(desire, min_depth))
      shm.desires.depth.set(min_depth)
    if subtask.finished:
      self.finish()

class GrabHeading(Task):
  def on_run(self):
    heading = shm.kalman.heading.get()
    self.logw('Grabbed current sub heading of {}'.format(heading))
    shm.buoys_mission.heading.set(heading)
    self.finish()

class GrabPosition(Task):
  def on_run(self):
    pos = aslam.sub.position()
    self.logw('Grabbed position of N {}, E {}, D {}'.format(pos[0], pos[1], pos[2]))
    shm.buoys_mission.north.set(pos[0])
    shm.buoys_mission.east.set(pos[1])
    shm.buoys_mission.depth.set(pos[2])
    self.finish()

class RestoreHeading(Task):
  def on_first_run(self):
    self.saved = shm.buoys_mission.heading.get()
    self.logw('Restoring sub heading of {}'.format(self.saved))

  def on_run(self):
    task = Heading(self.saved)
    task()
    if task.finished:
      self.finish()

class RestorePosition(Task):
  def on_first_run(self):
    self.saved = [shm.buoys_mission.north.get(), shm.buoys_mission.east.get(), shm.buoys_mission.depth.get()]
    self.logw('Restoring saved position of N {}, E {}, D {}'.format(*self.saved))

  def on_run(self):
    task = GoToPosition(self.saved[0], self.saved[1], depth = self.saved[2])
    task()
    if task.finished:
      self.finish()

Scan = Sequential(
  MoveYRough(2.0),
  MoveYRough(-4.0),
  MoveYRough(4.0),
  MoveYRough(-4.0),
  MoveYRough(2.0)
)

boundingBox = lambda pos: (pos - n.array([0.2, 0.2, 0.2]), pos + n.array([0.2, 0.2, 0.2]))

tolerance = n.array([0.05, 0.05, 0.05])

class TouchGuarded(Task):
  def on_run(self, subtask, sensor):
    subtask()
    if subtask.finished or not sensor.get():
      self.finish()

class AvoidYellow(Task):
  def on_first_run(self):
    self.heading        = shm.kalman.heading.get()
    self.red_buoy       = aslam.world.red_buoy.position()[:2]
    self.green_buoy     = aslam.world.green_buoy.position()[:2]
    self.yellow_buoy    = aslam.world.yellow_buoy.position()[:2]
    self.all_buoys      = [('red', self.red_buoy), ('green', self.green_buoy), ('yellow', self.yellow_buoy)]
    self.sorted_buoys   = sorted(self.all_buoys, key = lambda x: rotate(x[1], -self.heading)[1])
    self.logi('Sorted buoys (left-to-right): {}'.format([x[0] for x in self.sorted_buoys]))
    subtasks = []
    subtasks.append(MasterConcurrent(HPRWiggle(), MoveXRough(-1.0)))
    subtasks.append(Depth(PIPE_SEARCH_DEPTH))
    if self.sorted_buoys[0][0] == 'yellow':
        # yellow buoy far left, go right
        subtasks.append(MoveYRough(1.0))
    elif self.sorted_buoys[1][0] == 'yellow':
        subtasks.append(MoveYRough(1.0))
    else:
        subtasks.append(MoveYRough(-1.0))
    subtasks.append(MoveXRough(1.0))
    center_buoy = n.array(self.sorted_buoys[1][1])
    center_buoy += n.array(rotate((1, 0), self.heading)) # 1m beyond center buoy
    subtasks.append(GoToPosition(center_buoy[0], center_buoy[1], depth=PIPE_SEARCH_DEPTH))
    self.subtask = Sequential(*subtasks)

  def on_run(self):
    self.subtask()
    if self.subtask.finished:
        self.finish()

class AllBuoys(Task):
  def desiredModules(self):
    return [shm.vision_modules.Buoys]

  def on_first_run(self):

    self.has_made_progress = False
    shm.navigation_settings.optimize.set(False)

    delta_red = aslam.world.red_buoy.position() - aslam.sub.position()
    delta_red /= n.linalg.norm(delta_red)
    delta_red *= -1
    delta_green = aslam.world.green_buoy.position() - aslam.sub.position()
    delta_green /= n.linalg.norm(delta_green)
    delta_green *= -1
    delta_yellow = aslam.world.yellow_buoy.position() - aslam.sub.position()
    delta_yellow /= n.linalg.norm(delta_yellow)
    delta_yellow *= -1

    subtasks = []
    # subtasks.append(GoToPipe())
    subtasks.append(MoveXRough(PIPE_TO_BUOYS_DIST))
    subtasks.append(Depth(BUOY_SEARCH_DEPTH))
    subtasks.append(GrabPosition())
    subtasks.append(GrabHeading())
    subtasks.append(Scan)
    if 1:
      subtasks += [
        Timeout(20.0, SequentialSuccess(
          aslam.Target(aslam.world.red_buoy, delta_red, tolerance, boundingBox(delta_red * 2), orient = True)),
          RelativeToInitialDepth(0.05),
          Timeout(5.0, TouchGuarded(MoveXRough(1.3), shm.gpio.wall_1)),
        ),
        RestorePosition(),
        RestoreHeading()
      ]

    if 1:
      subtasks += [
        Timeout(20.0, SequentialSuccess(
          aslam.Target(aslam.world.green_buoy, delta_green, tolerance, boundingBox(delta_green * 2), orient = True)),
          RelativeToInitialDepth(0.1),
          Timeout(5.0, TouchGuarded(MoveXRough(1.3), shm.gpio.wall_1)),
        ),
        RestorePosition(),
        RestoreHeading()
      ] 
    
    if 1:
      subtasks += [
        Timeout(20.0, SequentialSuccess(
          aslam.Target(aslam.world.yellow_buoy, delta_yellow, tolerance, boundingBox(delta_yellow * 2), orient = True)),
          RelativeToInitialDepth(0.1),
          GuardedTimer(10.0, Scuttle(), aslam.SimpleTarget(aslam.world.yellow_buoy, delta_yellow)),
        ),
        RestorePosition(),
        RestoreHeading(),
        AvoidYellow()
      ]
  
    subtasks.append(RestoreHeading())

    self.subtask = MinDepth(0.1, Sequential(*subtasks))

  def on_run(self):
    self.subtask()
    if self.subtask.finished:
      self.finish()
