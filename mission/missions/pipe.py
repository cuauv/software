#!/usr/bin/env python3

import math

import shm

from mission.constants.config import PIPE_SEARCH_DEPTH, PIPE_FOLLOW_DEPTH, PIPE_RIGHT_FIRST
from mission.framework.combinators import Sequential, Concurrent, Retry, Conditional
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY
from mission.framework.position import PositionalControl
from mission.framework.primitive import Zero, Log, FunctionTask, Fail
from mission.framework.search import SearchFor, VelocityTSearch, SwaySearch, PitchSearch
from mission.framework.targeting import DownwardTarget, PIDLoop
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.jank import TrackMovementY, RestorePosY

class center(Task):
    def update_data(self):
        self.pipe_results = shm.pipe_results.get()

    def on_first_run(self):
        self.update_data()

        pipe_found = self.pipe_results.heuristic_score > 0

        self.centered_checker = ConsistencyCheck(8, 10)

        self.center = DownwardTarget(lambda self=self: (self.pipe_results.center_x, self.pipe_results.center_y),
                                     target=get_downward_camera_center,
                                     deadband=(30,30), px=0.002, py=0.002, dx=0.002, dy=0.002,
                                     valid=pipe_found)
        #self.logi("Beginning to center on the pipe")


    def on_run(self):
        self.update_data()
        self.center()
        #self.logi("Results Y: {}".format(str(self.pipe_results.center_y)))
        #self.logi("Center: {}".format(str(get_downward_camera_center()[1])))

        if not check_seen():
            self.finish(success=False)

        if self.centered_checker.check(self.center.finished):
            self.center.stop()
            self.finish()

class align(Task):
    def update_data(self):
        self.pipe_results = shm.pipe_results.get()

    def on_first_run(self):
        self.update_data()

        self.align = Heading(lambda: self.pipe_results.angle + shm.kalman.heading.get(), deadband=0.5)
        self.alignment_checker = ConsistencyCheck(49, 50)

        pipe_found = self.pipe_results.heuristic_score > 0

        self.center = DownwardTarget(lambda self=self: (self.pipe_results.center_x, self.pipe_results.center_y),
                                     target=get_downward_camera_center,
                                     deadband=(10,10), px=0.001, py=0.001, dx=0.002, dy=0.002,
                                     valid=pipe_found)

        #self.logi("Beginning to align to the pipe's heading")

    def on_run(self):
        self.update_data()

        self.align()
        self.center()

        if not check_seen():
            self.finish(success=False)

        if self.alignment_checker.check(self.align.finished):
            self.finish()

search_task_behind = lambda: SearchFor(VelocityTSearch(forward=2,stride = 3, rightFirst=PIPE_RIGHT_FIRST, checkBehind=True),
                                lambda: shm.pipe_results.heuristic_score.get() > 0,
                                consistent_frames=(10, 10))

search_task= lambda: SearchFor(VelocityTSearch(forward=2,stride = 3, rightFirst=PIPE_RIGHT_FIRST),
                                lambda: shm.pipe_results.heuristic_score.get() > 0,
                                consistent_frames=(10, 10))

pitch_search_task = lambda: SearchFor(PitchSearch(30),
                                      lambda: shm.pipe_results.heuristic_score.get() > 0,
                                      consistent_frames=(6, 6))

pipe_test = lambda: Sequential(Depth(PIPE_SEARCH_DEPTH),
                               search_task(), center(), align(),
                               Depth(PIPE_FOLLOW_DEPTH))

pitch_pipe_test = lambda: Sequential(Depth(PIPE_SEARCH_DEPTH),
                          pitch_search_task(), Zero(),
                          Concurrent(center(), Pitch(0)), Zero(),
                          center(), align(), Depth(PIPE_FOLLOW_DEPTH))

def check_seen():
    visible = shm.pipe_results.heuristic_score.get()

    #print(visible)
    if visible > 0:
        return True
    else:
        #print('Lost Pipe!')
        return False

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

def one_pipe(grp):
    return Timeout(45, Sequential(
        Log('Going to Search Depth'),
        Depth(PIPE_SEARCH_DEPTH),
        Zero(),
        Log('Sway searching for pipe with Behind'),
        TrackMovementY(search_task_behind()),
          Retry(lambda: Sequential(
            Zero(),
            Log('Sway searching for pipe; may have been lost'),
            TrackMovementY(search_task(), shm.jank_pos.y.get()),
            Log('Centering on pipe'),
            Conditional(
              center(),

              on_fail=Fail(Sequential(
                Log("Pipe lost, Attempting to Restore Y pos"),
                Zero(),
                TrackMovementY(RestorePosY(.3), shm.jank_pos.y.get()),
              ))
            ),

            Depth(PIPE_FOLLOW_DEPTH),
            Log('Aligning with pipe'),
            Concurrent(
                align(),
                center(),
                finite=False
            ),
            ), float("inf")),

        Zero(),
        Log('Aligned, moving forward'),
        Timed(VelocityX(.4),3),
        Zero()
        #Depth(PIPE_FOLLOW_DEPTH)
    ))

full = one_pipe(shm.desires)

def second_pipe(grp):
    return Sequential(
        Log('Going to Search Depth'),
        Depth(PIPE_SEARCH_DEPTH),
        Retry(lambda: Sequential(
            Zero(),
            Log('Sway searching for pipe'),
            search_task(),
            Log('Centering on pipe'),
            center(),

            Depth(PIPE_FOLLOW_DEPTH),
            Log('Aligning with pipe'),
            Concurrent(
                align(),
                center(),
                finite=False
            ),
            ), float("inf")),
        Zero(),
        Log('Aligned, moving forward'),
        Timed(VelocityX(.4),3),
        Zero()
        #Depth(PIPE_FOLLOW_DEPTH)
    )

#pipes_mission = second_pipe(shm.desires)

def pitch_pipe(grp):
    return Sequential(
             Depth(PIPE_SEARCH_DEPTH),
             pitch_search_task(),
             Zero(),
             center(),
             Pitch(0),
             center(),

             Depth(PIPE_FOLLOW_DEPTH),

             Concurrent(
                 #center(),
                 align(),
                 finite=False,
             ),
             PositionalControl(),
             Zero(),
    )

pitch_pipe_mission = pitch_pipe(shm.desires)



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
