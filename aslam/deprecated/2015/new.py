#!/usr/bin/env python3.4

from slam import *

world['sub'].zero()

initialize()

print('Sub: {0} Gate: {1}'.format(world['sub'].position(), world['gate'].position()))

world['sub'].distanceTo(world['gate'], 0.0) # unclear

print('Sub: {0} Gate: {1}'.format(world['sub'].position(), world['gate'].position()))
