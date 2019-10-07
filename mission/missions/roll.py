from mission.framework.movement import RelativeToInitialHeading, Depth, VelocityX, VelocityY, RelativeToCurrentRoll, RelativeToInitialRoll, Roll
from mission.framework.timing import Timed, Timer
from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, While
from mission.framework.primitive import Zero, Log, FunctionTask, Fail, NoOp
from mission.framework.task import Task
import shm

def store_p(v):
    global pv
    pv = v

def get_pv():
    global pv
    return pv

def functionalizer(*args): pass

class RollDegrees(Task):
    def on_first_run(self, N):
        #self.target_degrees = N
        self.current_r = shm.kalman.roll.get()
        self.progress = 0
    def on_run(self, N):
        new_angle = shm.kalman.roll.get()
        angle_diff = new_angle - self.current_r
        self.current_r = new_angle
        Ad = ((angle_diff + 180) % 360) - 180
        self.progress += Ad if N > 0 else -Ad
        if self.progress > abs(N): self.finish()

pv = shm.settings_roll.kP.get()
roll = lambda: Sequential(
        FunctionTask(lambda: shm.settings_roll.kP.set(.6)),
        #*([RelativeToInitialRoll(90)] * 8),
        #Timed(RelativeToCurrentRoll(90), 6),
        MasterConcurrent(RollDegrees(720 - 45), RelativeToCurrentRoll(90), VelocityX(.35)),
        #MasterConcurrent(RollDegrees(360), RelativeToCurrentRoll(90), VelocityX(.35)),
        #Timer(.5),
        #MasterConcurrent(RollDegrees(360 - 45), RelativeToCurrentRoll(90), VelocityX(.35)),
        #MasterConcurrent(RollDegrees(-720 + 45), RelativeToCurrentRoll(-90)),
        FunctionTask(lambda: shm.settings_roll.kP.set(pv)),
        Concurrent(
            Roll(0),
            VelocityX(0),
        ),
        Timer(2),
)
t2 = Timed(RelativeToCurrentRoll(-90), 3)

