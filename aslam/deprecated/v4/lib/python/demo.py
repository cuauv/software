#!/usr/bin/env python3

import random
import aslam
from aslam import Expr as E
from aslam import Type as T
from aslam import BuiltIn as B
from aslam import Estimate as EST

env = aslam.Environment('testing')

gaussian = lambda val: E.LAMBDA([('val', T.REAL())], E.LIFT(0.001) + E.APPLY(E.LIFT(B.GAUSSIAN), [E.VAR('val'), E.LIFT(val), E.LIFT(0.1)]))

'''
# SIMPLE SYS EQS

env.variable('x', {'asgrid': [(-5, 5), (-5, 5), (-5, 5)]}, resample = {'spread': 3, 'uncer': 3.})

print(env.estimate(E.VAR('x'), EST.ARGMAX))

# x = 1, y = -1, z = 0

# x + y = 0
env.observe_constant(E.VAR('x')[0] + E.VAR('x')[1], gaussian(0.))

# x - y = 2
env.observe_constant(E.VAR('x')[0] - E.VAR('x')[1], gaussian(2.))

# x + z = 1
env.observe_constant(E.VAR('x')[0] + E.VAR('x')[2], gaussian(1.))

print(env.estimate(E.VAR('x'), EST.ARGMAX))
'''

# THE BUOY

env.variable('lightPos', {'asgrid': [(0, 40), (0, 40)]}, granularity = 160, resample = {'spread': 3, 'uncer': 0.01})

distance = lambda x, y: ( (x[0] - y[0]) ** E.LIFT(2.) + (x[1] - y[1]) ** E.LIFT(2.) ) ** E.LIFT(1. / 2)

pixelLum = lambda coord: E.LIFT(100) / (E.LIFT(1.) + distance(E.VAR('lightPos'), coord))

real = (5, 35)
dist = lambda x, y: ( ( (x[0] - y[0]) ** 2. ) + ( (x[1] - y[1]) ** 2. ) ) ** (1. / 2)

print(env.estimate(E.VAR('lightPos'), EST.ARGMAX))
print(env.estimate(E.VAR('lightPos'), EST.WMEAN))

for x in range(40):
  for y in range(40):
    x = x * 1.
    y = y * 1.
    lum = (100. / (1. + dist((x, y), real))) + ( random.random() - 0.5 )
    env.observe_constant(pixelLum(E.LIFT([x, y])), gaussian(lum))
    # print(x, y, lum, env.estimate(E.VAR('lightPos'), EST.ARGMAX))

print(env.estimate(E.VAR('lightPos'), EST.ARGMAX))
print(env.estimate(E.VAR('lightPos'), EST.WMEAN))
