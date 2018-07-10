# Pick up a golf ball from cash-in. Hopefully.

from mission.framework.task import Task
from mission.framework.combinators import Sequential
from mission.framework.movement import Depth
from mission.framework.primitive import Log, Zero
from mission.framework.timing import Timer

ATTEMPTS = 10
DELAY_UP = 1
DELAY_DOWN = 1.5

DEPTH_UP = 2
DEPTH_DOWN = 2.3

count = 0

class Functioner(Task):
    def on_run(self, function):
        function()
        self.finish()

def reset_counter():
    global counter
    counter = 0

def counter():
    global count
    count += 1

PickUp = Sequential(
    Depth(DEPTH_UP),
    Zero(),
    Functioner(reset_counter),
    *([
        Functioner(counter),
        Log('Picking up {}'.format(count)),
        Timer(DELAY_UP),
        Depth(DEPTH_DOWN),
        Timer(DELAY_DOWN),
        Depth(DEPTH_UP),
    ] * ATTEMPTS),
    Depth(DEPTH_UP),
    Zero(),
)
