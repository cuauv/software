#!/usr/bin/env python3

import math

import shm

from mission.constants.config import PIPE_SEARCH_DEPTH, PIPE_FOLLOW_DEPTH
from mission.framework.task import Task
from mission.framework.combinators import Sequential
from mission.framework.movement import Depth, Heading
from mission.framework.search import SearchFor, SwaySearch
from mission.framework.targeting import DownwardTarget, PIDLoop
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.timing import Timer

class center(Task):
    def update_data(self):
        self.pipe_results = shm.pipe_results.get()

    def on_first_run(self):
        self.update_data()

        pipe_found = self.pipe_results.heuristic_score > 0

        self.centered_checker = ConsistencyCheck(18, 20)

        self.center = DownwardTarget(lambda self=self: (self.pipe_results.center_x * 100, self.pipe_results.center_y * 100),
                                     target= (0,0),
                                     deadband=(4,4), px=0.03, py=0.03, dx=0.01, dy=0.01,
                                     valid=pipe_found)
        self.logi("Beginning to center on the pipe")

        self.logi("Pipe center" + str(self.pipe_results.center_x) + " " +str(self.pipe_results.center_y))

    def on_run(self):
        self.update_data()
        self.center()

        if self.centered_checker.check(self.center.finished):
            self.center.stop()
            self.finish()

class align(Task):
    def update_data(self):
        self.pipe_results = shm.pipe_results.get()

    def on_first_run(self):
        self.update_data()

        self.align = Heading(lambda: self.pipe_results.angle + shm.kalman.heading.get(), deadband=0.5)
        self.alignment_checker = ConsistencyCheck(19, 20)

        pipe_found = self.pipe_results.heuristic_score > 0

        self.center = DownwardTarget(lambda self=self: (self.pipe_results.center_x * 100, self.pipe_results.center_y * 100),
                                     target=(0,0),
                                     deadband=(1,1), px=0.03, py=0.03, dx=0.01, dy=0.01,
                                     valid=pipe_found)

        self.logi("Beginning to align to the pipe's heading")

        c_center = "Camera Center: " + str((.5,.5))
        self.logi("Pipe center" + str(self.pipe_results.center_x) + " " +str(self.pipe_results.center_y))
        self.logi(c_center)

    def on_run(self): 
        self.update_data()

        self.align()
        self.center()

        if self.alignment_checker.check(self.align.finished):
            self.finish()

class mark(Task):
    def on_run(self, grp):
        cur = grp.get()
        cur.north = shm.kalman.north.get()
        cur.east = shm.kalman.east.get()
        cur.depth = shm.kalman.depth.get()
        cur.heading = math.radians(shm.pipe_results.angle.get())
        grp.set(cur)
        self.finish()

search_task = lambda: SearchFor(SwaySearch(1, 1),
                                lambda: shm.pipe_results.heuristic_score.get() > 0,
                                consistent_frames=(6, 6))

pipe_test = lambda: Sequential(Depth(PIPE_SEARCH_DEPTH),
                               search_task(), center(), align(),
                               Depth(PIPE_FOLLOW_DEPTH))
one_pipe = lambda grp: Sequential(Depth(PIPE_SEARCH_DEPTH),
                              search_task(), center(), align(), mark(grp),
                              Depth(PIPE_FOLLOW_DEPTH))
pipe_mission = Sequential(one_pipe(shm.desires), one_pipe(shm.desires))

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

class OptimizablePipe(Task):
  def desiredModules(self):
    return [shm.vision_modules.Pipes]

  def on_first_run(self, grp):
    self.subtask = one_pipe(grp)
    self.has_made_progress = False

  def on_run(self, grp):
    self.subtask()
    if self.subtask.finished:
      self.finish()
