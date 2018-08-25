from mission.framework.task import Task
from mission.framework.movement import RelativeToCurrentHeading

spin_speed = 90

spin_right = RelativeToCurrentHeading(spin_speed)
spin_left = RelativeToCurrentHeading(-spin_speed)
