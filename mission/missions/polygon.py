from mission.framework.combinators import Sequential, Concurrent
from mission.framework.movement import RelativeToInitialHeading
from mission.framework.position import MoveX
from mission.framework.primitive import Log

sides = 8

tasks = list(map(
    lambda i: Sequential(Log("Beginning side {}".format(i)), MoveX(4 / sides), RelativeToInitialHeading(360 / sides)),
    range(sides)
))

print(tasks)

polygon = Sequential(subtasks=tasks)
