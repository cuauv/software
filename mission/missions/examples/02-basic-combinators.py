from mission.framework.combinators import Concurrent, Sequential
from mission.framework.primitive import Depth, Heading, Roll
from mission.framework.timing import Timer

seq = Sequential(Depth(0),
                 Heading(180),
                 Roll(180),
                 Timer(2))

con = Concurrent(Depth(1),
                 Heading(90),
                 Roll(-90),
                 Timer(2))
