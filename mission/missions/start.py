import shm

from mission.framework.movement import Heading
from mission.framework.task import Task

WAIT_AFTER_GO = 3.0

class WaitForUnkill(Task):
  def on_first_run(self):
    self.first_triggered_time = None
    self.logw('Waiting for unhardkill! Will wait for %0.2fs of continuous switch' % WAIT_AFTER_GO)

  def on_run(self):
    if not shm.switches.hard_kill.get():
      if self.first_triggered_time is None:
        self.logw('Unhardkill trigger started!')
        self.first_triggered_time = self.this_run_time
        Heading()(shm.kalman.heading.get())
    
      if self.this_run_time - self.first_triggered_time > WAIT_AFTER_GO:
        self.logw('Received %0.2fs continuous unhardkill, STARTING MISSION!' % WAIT_AFTER_GO)
        shm.switches.soft_kill.set(0)
        self.finish()

    else:
      self.first_triggered_time = None

class WaitForMissionStart(Task):
  def on_first_run(self):
    self.first_triggered_time = None
    self.logw('Waiting for mission start! Will wait for one second of continuous switch')

  def on_run(self):
    if shm.merge_status.mission_start.get():
      if self.first_triggered_time is None:
        self.logw('Mission start trigger started!')
        self.first_triggered_time = self.this_run_time
    
      if self.this_run_time - self.first_triggered_time > 1.0:
        self.logw('Received 1.0s continuous mission start, STARTING MISSION!')
        self.finish()

    else:
      self.first_triggered_time = None
