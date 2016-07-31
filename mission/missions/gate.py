import shm

from mission.framework.combinators import Sequential
from mission.framework.movement import Depth, Heading
from mission.framework.position import MoveX
from mission.missions.start import WaitForUnkill
from mission.constants.region import GATE_DISTANCE

gate = Sequential(Depth(1.0), MoveX(GATE_DISTANCE))
