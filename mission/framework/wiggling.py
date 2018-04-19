from mission.framework.task import *
from mission.framework.position import *
from mission.framework.movement import *
from mission.framework.timing import *
from mission.framework.combinators import *

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

class ConfigurableWiggle(Task):
    def on_first_run(self, wiggle_amount, num_wiggles, wiggle_x, wiggle_y, wiggle_z):
        tasks = []
        if wiggle_x:
          tasks.append(MoveX(-wiggle_amount / 2., wiggle_amount / 1.5))
        if wiggle_y:
          tasks.append(MoveY(-wiggle_amount / 2., wiggle_amount / 1.5))
        if wiggle_z:
          tasks.append(RelativeToInitialDepth(- wiggle_amount / 1.5))
        for num in range(num_wiggles):
          negated = num % 2 == 1
          amount  = (-1 if negated else 1) * wiggle_amount
          if wiggle_x: tasks.append(MoveX(amount, wiggle_amount / 1.5))
          if wiggle_y: tasks.append(MoveY(amount, wiggle_amount / 1.5))
          if wiggle_z: tasks.append(RelativeToInitialDepth(amount))
        if wiggle_x:
          tasks.append(MoveX(wiggle_amount / 2., wiggle_amount / 1.5))
        if wiggle_y:
          tasks.append(MoveY(wiggle_amount / 2., wiggle_amount / 1.5))
        if wiggle_z:
          tasks.append(RelativeToInitialDepth(wiggle_amount / 2., wiggle_amount / 1.5))

        self.task = Sequential(*tasks)

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

BigWiggle     = lambda x, y, z: ConfigurableWiggle(0.25, 10, x, y, z)
MediumWiggle  = lambda x, y, z: ConfigurableWiggle(0.15, 5, x, y, z)
SmallWiggle   = lambda x, y, z: ConfigurableWiggle(0.10, 3, x, y, z)

SmallXWiggle    = SmallWiggle(True, False, False)
SmallYWiggle    = SmallWiggle(False, True, False)
SmallZWiggle    = SmallWiggle(False, False, True)
SmallXYWiggle   = SmallWiggle(True, True, False)
SmallXZWiggle   = SmallWiggle(True, False, True)
SmallYZWiggle   = SmallWiggle(False, True, True)
SmallXYZWiggle  = SmallWiggle(True, True, True)

MediumXWiggle    = MediumWiggle(True, False, False)
MediumYWiggle    = MediumWiggle(False, True, False)
MediumZWiggle    = MediumWiggle(False, False, True)
MediumXYWiggle   = MediumWiggle(True, True, False)
MediumXZWiggle   = MediumWiggle(True, False, True)
MediumYZWiggle   = MediumWiggle(False, True, True)
MediumXYZWiggle  = MediumWiggle(True, True, True)

BigXWiggle    = BigWiggle(True, False, False)
BigYWiggle    = BigWiggle(False, True, False)
BigZWiggle    = BigWiggle(False, False, True)
BigXYWiggle   = BigWiggle(True, True, False)
BigXZWiggle   = BigWiggle(True, False, True)
BigYZWiggle   = BigWiggle(False, True, True)
BigXYZWiggle  = BigWiggle(True, True, True)

def HPRWiggle():
  iteration = lambda negate: Timeout(3.0, MasterConcurrent(
    Pitch(-20 if negate else 20),
    RelativeToInitialHeading(-20 if negate else 20),
    Roll(-20 if negate else 20)
    ))
  tasks = [iteration(n % 2 == 1) for n in range(6)]
  tasks.append(Pitch(0))
  tasks.append(Roll(0))
  return Sequential(*tasks)
