import shm

from mission.framework.movement import Heading
from mission.framework.task import Task
from sensors.kalman.set_zero_heading import set_zero_heading

class WaitForUnkill(Task):
  def on_first_run(self, killed=True, wait=3.0):
    self.first_triggered_time = None
    self.WAIT_AFTER_GO = wait
    self.logw('Waiting for (un)hardkill! Will wait for %0.2fs of continuous switch' % self.WAIT_AFTER_GO)

  def on_run(self, killed=True, wait=3.0):
    if (killed and not shm.switches.hard_kill.get()) or (not killed and shm.switches.hard_kill.get()):
      if self.first_triggered_time is None:
        if killed:
          self.logw('Unhardkill trigger started!')
          self.logi("Setting Zero Heading")
          set_zero_heading()
        else:
          self.logw('Hardkill trigger started!')
        self.first_triggered_time = self.this_run_time

      if self.this_run_time - self.first_triggered_time > self.WAIT_AFTER_GO:
        self.logw('Received %0.2fs continuous unhardkill, STARTING MISSION!' % self.WAIT_AFTER_GO)
        #shm.deadman_settings.enabled.set(False)
        #shm.switches.soft_kill.set(0)
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
