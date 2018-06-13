from mission.framework.task import *
from mission.framework.movement import *
from mission.framework.primitive import *
from mission.missions.start import *
from mission.framework.combinators import *
from mission.framework.timing import *
from mission.missions.buoys import red
from mission.missions.pipe import loki_pipe

from auv_python_helpers.angles import *

import datetime
import math
import shm
import os

class VelocitySpiral(Task):
  def on_run(self):
    theta = (self.this_run_time - self.first_run_time) * 20 # 10 degrees per second
    ratio = 1 + round(math.floor(theta / 360))
    theta /= ratio
    theta = theta % 360
    task = Heading(theta, deadband = 15.)
    task()
    if abs(heading_sub_degrees(shm.kalman.heading.get(), shm.desires.heading.get())) < 10.0:
      VelocityX(1.0)()
    else:
      VelocityX(0.0)()

link_stage_path = os.path.join(os.environ['CUAUV_SOFTWARE'], 'link-stage')

Call = lambda name: FunctionTask(lambda: os.system('{}/{}'.format(link_stage_path, name)))

Forward = lambda time: Sequential(Timed(VelocityX(1.0), time), VelocityX(0.0))
RedBuoy = red
Gate    = lambda: Sequential(Depth(1.0, deadband = 0.05), Timed(VelocityX(1.0), 15.0), VelocityX(0.0))
Pipe    = lambda: loki_pipe

FullSwitched = Sequential(
  EnableController(),
  WaitForUnkill(),
  HardkillGuarded(Sequential(
    FunctionTask(lambda: shm.vision_modules.Record.set(True)),
    Call('auv-shmlogd -o {}/shmlog-{}.shmlog &'.format(os.environ['CUAUV_SOFTWARE'], datetime.datetime.now().strftime('%Y-%m-%d-%H-%M-%S'))),
    Gate(),
    Forward(2),
    Pipe(),
    Forward(15),
    Pipe(),
    Forward(30),
    Depth(1.0),
    Heading(-180),
    Timer(33000)
  ))
)
