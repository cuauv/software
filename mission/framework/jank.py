#!/usr/bin/env python3

import math
import shm

from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY
from mission.framework.primitive import Zero, Log, FunctionTask, Fail
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed

'''
Oh no you're using jank.
Usage:
	Mini Sub: roughly tracks sub position in undefined units while passed task is run
	Main Sub: please dont im begging you. we have a dvl for a reason
'''

class TrackMovementY(Task):
    def on_first_run(self, task, startPos=0, *args, **kwargs):
      self.vel_add = float(startPos)

    def on_run(self, task, *args, **kwargs):
      task()
      self.vel_add += float(shm.desires.sway_speed.get())
      shm.jank_pos.y.set(self.vel_add)
      if task.finished:
        self.finish()

class TrackMovementX(Task):
    def on_first_run(self, task, startPos=0, *args, **kwargs):
      self.vel_add = float(startPos)

    def on_run(self, task, *args, **kwargs):
      task()
      self.vel_add += float(shm.desires.speed.get())
      shm.jank_pos.x.set(self.vel_add)
      if task.finished:
        self.finish()

class TrackMovementXY(Task):
    def on_first_run(self, task, startPosX=0, startPosY=0, *args, **kwargs):
      self.vel_add_y = float(startPosY)
      self.vel_add_x = float(startPosX)

    def on_run(self, task, *args, **kwargs):
      task()
      self.vel_add_x += float(shm.desires.speed.get())
      self.vel_add_y += float(shm.desires.sway_speed.get())
      shm.jank_pos.x.set(self.vel_add_x)
      shm.jank_pos.y.set(self.vel_add_y)
      if task.finished:
        self.finish()

class RestorePosY(Task):
  def on_first_run(self, vel, *args, **kwargs):
    if shm.jank_pos.y.get() < 0:
      self.task = VelocityY(vel)
    else:
      self.task = VelocityY(vel * -1)

  def on_run(self, vel, *args, **kwargs):
    if abs(shm.jank_pos.y.get()) > vel :
      self.task()
    else:
      self.logi("Y pos restored, I hope")
      Zero()
      self.finish()

class RestorePosX(Task):
  def on_first_run(self, vel, *args, **kwargs):
    if shm.jank_pos.x.get() < 0:
      self.task = VelocityX(vel)
    else:
      self.task = VelocityX(vel * -1)

  def on_run(self, vel, *args, **kwargs):
    if abs(shm.jank_pos.x.get()) > vel :
      self.task()
    else:
      self.logi("X pos restored, I hope")
      Zero()
      self.finish()

#just to test stuff
def testRightRestore():
  return TrackMovementY(Sequential(
    Timed(VelocityY(.4), 6),
    Log('Restoring?'),
    RestorePosY(.2),
    Zero(),
    ))
def testLeftRestore():
  return TrackMovementY(Sequential(
    Timed(VelocityY(-.4), 6),
    Log('Restoring?'),
    RestorePosY(.2),
    Zero(),
    ))