# Fire actuators!

from mission.framework.task import Task
from mission.framework.combinators import Sequential
from mission.framework.timing import Timer
from mission.framework.primitive import FunctionTask, Log

import shm

from mission.constants.sub import PISTONS, TORPEDOES, PISTON_DELAY, TORPEDO_DELAY

Actuate = lambda channel, value: FunctionTask(lambda: channel.set(value))

FirePiston = lambda piston: Sequential(
    # Open
    Actuate(piston[1], 0),
    Actuate(piston[0], 1),
    Timer(PISTON_DELAY),
    # Close
    Actuate(piston[0], 0),
    Actuate(piston[1], 1),
    Timer(PISTON_DELAY),
    # Reset
    Actuate(piston[1], 0),
    Timer(PISTON_DELAY),
)

FireTorpedo = lambda torpedo: Sequential(
    # Fire
    Actuate(torpedo, 1),
    Timer(TORPEDO_DELAY),
    # Reset
    Actuate(torpedo, 0),
)

# We need to force-retract the piston before trying to pick things up
InitPiston = lambda piston: Actuate(piston[1], 1)

ResetPiston = lambda piston: Sequential(
    Actuate(piston[0], 0),
    Actuate(piston[1], 0),
)

# Don't actually use this
InitPistons = lambda: Sequential(
    *[InitPiston(piston) for name, piston in PISTONS.items()],
    Timer(PISTON_DELAY),
)

ResetPistons = lambda: Sequential(
    *[ResetPiston(piston) for name, piston in PISTONS.items()],
    Timer(PISTON_DELAY),
)

FireGreen = lambda: FirePiston(PISTONS['green'])
FireRed = lambda: FirePiston(PISTONS['red'])
FireBlue = lambda: FirePiston(PISTONS['blue'])

FireTorpedoA = lambda: FireTorpedo(TORPEDOES[0])
FireTorpedoB = lambda: FireTorpedo(TORPEDOES[1])

TestPistons  = lambda: Sequential(
    Log('Firing green...'),
    FireGreen(),
    Log('Firing red...'),
    FireRed(),
    Log('Firing blue...'),
    FireBlue(),
    Log('Done'),
)

TestTorpedoes = lambda: Sequential(
    Log('Firing torpedo A...'),
    FireTorpedoA(),
    Log('Firing torpedo B...'),
    #FireTorpedoB(), # re-enable once we get a second torpedo tube
    Log('Done'),
)
