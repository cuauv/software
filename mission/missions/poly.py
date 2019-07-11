from mission.framework.combinators import Sequential, Retry, While
from mission.framework.movement import RelativeToInitialHeading, VelocityY, VelocityX
from mission.framework.position import MoveY
from mission.framework.primitive import Log, Fail, Succeed, FunctionTask
from mission.framework.timing import Timed

from mission.missions.will_common import FakeMoveY

sides = 6

def loop_state():
    current = -1
    def iterate():
        nonlocal current
        current += 1
        return current < sides
    return iterate

polygon = Sequential(
    While(
        lambda: Sequential(
            Timed(VelocityY(-0.2), 6),
            RelativeToInitialHeading(360 / sides),
            Timed(VelocityY(-0.2), 6)
            ), loop_state()
    )
)
