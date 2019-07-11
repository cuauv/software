from mission.framework.combinators import Sequential
from mission.framework.position import MoveX, MoveY
from mission.framework.movement import RelativeToInitialHeading, VelocityX, VelocityY, Depth
from mission.framework.timing import Timed
from mission.framework.primitive import Zero, Log


MainSub = Sequential(Depth(1.5), 
        Log('Moving Forward'),
        MoveX(4.25, deadband=0.4), 
        Log('Moving Horizontal'),
        MoveY(0.6, deadband=0.3), 
        Log('Moving Backwards'),
        MoveX(-4.25, deadband=0.4))

MiniSub = Sequential(Depth(2), 
        Timed(VelocityX(0.4), 25), Zero(), 
        Timed(VelocityY(0.2), 10), Zero(), 
        Timed(VelocityX(-0.4), 25), Zero())

Move = MoveX(1, deadband=0.2)
