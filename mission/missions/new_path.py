#!/usr/bin/env python3

import math

import shm

from mission.framework.combinators import Sequential, Concurrent, Retry, Conditional, While
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY, RelativeToCurrentHeading
from mission.framework.position import PositionalControl
from mission.framework.primitive import Zero, Log, FunctionTask, Fail
from mission.framework.search import SearchFor, VelocityTSearch, SwaySearch, PitchSearch
from mission.framework.targeting import DownwardTarget, PIDLoop, HeadingTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.jank import TrackMovementY, RestorePosY

PATH_FOLLOW_DEPTH = .6
PATH_SEARCH_DEPTH = .6
PATH_RIGHT_FIRST = True

def check_seen(results):
    visible = results.visible.get()

    #print(visible)
    if visible > 0:
        return True
    else:
        #print('Lost Path!')
        return False


class first_center(Task):

    def on_first_run(self, results):
        self.update_data(results)

        path_found = self.path_results.visible > 0

        self.centered_checker = ConsistencyCheck(8, 10)

        self.center = DownwardTarget(lambda self=self: (self.path_results.center_x, self.path_results.center_y),
                                     target=(0,0),
                                     deadband=(.05,.05), px=0.5, py=0.5, dx=0.02, dy=0.02,
                                     valid=path_found)

    def on_run(self, results):
        self.update_data(results)
        self.center()

        # if not check_seen(results):
        #     self.finish(success=False)
        if self.centered_checker.check(self.center.finished):
            self.center.stop()
            self.finish()

def checkNotAligned(results):
    #returns false until aligned
    c = abs(results.center_x) < .05 and abs(results.center_y) < .05
    a = abs(results.angle) < 95 and abs(results.angle) > 85
    print(abs(results.angle))
    print (abs(results.center_x))
    print(not (c and a))
    return not (c and a)


search_task_behind = lambda: SearchFor(VelocityTSearch(forward=2,stride = 3, rightFirst=PATH_RIGHT_FIRST, checkBehind=True),
                                lambda: shm.path_results_1.visible.get() > 0,
                                consistent_frames=(10, 10))

search_task= lambda: SearchFor(VelocityTSearch(forward=2,stride = 3, rightFirst=PATH_RIGHT_FIRST),
                                lambda: (shm.path_results_1.visible.get() > 0 and shm.path_results_2.visible.get() > 0),
                                consistent_frames=(10, 10))

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

def pathAngle(results):
    a = results.angle
    h = shm.kalman.heading.get()
    if abs(h - (a + 90)) > abs(h - (a - 90)):
        return h + a - 90
    else:
        return h + a + 90

def one_path(grp):
    return Timeout(90, Sequential(
        Zero(),
        search_task(),
        While(lambda: Sequential(
            Zero(),
            Log('Centering on path'),
            center(grp),
            Log('Going to follow depth'),
            Depth(PATH_FOLLOW_DEPTH),
            Log('Aligning with path'),
            Heading(pathAngle(grp.get()), deadband=0.1),
            Zero(),
            Timer(1),
            Log(grp.angle.get()),
        ), lambda: checkNotAligned(grp.get())),
        Log('aligned'),
        Zero(),
    ))

def both_paths():
    return Sequential(
        Log('Going to Search Depth'),
        Depth(PATH_SEARCH_DEPTH),
        Zero(),
        Log('Beginning to align to Path 1'),
        one_path(shm.path_results_1),
        Log('Moving forward'),
        Timed(VelocityX(.2), 1),
        Log('Beginning to align to Path 2'),
        one_path(shm.path_results_2),
        Log('Done'),
        Zero()
    )

full = both_paths()

class OptimizablePath(Task):
  def desiredModules(self):
    return [shm.vision_modules.Paths]

  def on_first_run(self, grp):
    self.subtask = test_path(grp)
    self.has_made_progress = False

  def on_run(self, grp):
    self.subtask()
    if self.subtask.finished:
      self.finish()
