#!/usr/bin/env python3

import sys; sys.path.append('libs/python')

from aslam.core import *
from aslam.model import *
from aslam.function import *
from aslam.parameter import *
from aslam.type import *
from aslam.repr import *
from aslam.object import *

import math

'''

# Simplest possible model: identity function.

identity = model(
  {'id': function({'x': boundedrealt(-5., 5.)}, var('x').raw)},
  {'obj': object(boundedrealt(-5., 5.), 0.9)},
  200
)

inst = Instance('identity', model = identity)

print(inst.observe('id', {'x': pobject('obj')}, realr(2.5), 0.1))

print(inst.estimate('obj'))

# Linear system of equations.

v = vectort([boundedrealt(-5., 5.), boundedrealt(-5., 5.), boundedrealt(-5., 5.)])

linear = model(
  {'f1': function({'v': v}, (var('v')[0] + var('v')[1] + var('v')[2]).raw),
   'f2': function({'v': v}, (var('v')[0] + var('v')[1] - var('v')[2]).raw),
   'f3': function({'v': v}, (var('v')[0] - var('v')[1] + var('v')[2]).raw)},
  {'obj': object(v, None)},
  500
)

inst2 = Instance('linear', model = linear)

# x = y = 1, z = 0

print(inst2.observe('f1', {'v': pobject('obj')}, realr(2.0), 0.1))
print(inst2.observe('f2', {'v': pobject('obj')}, realr(0.0), 0.1))
print(inst2.observe('f3', {'v': pobject('obj')}, realr(2.0), 0.1))

print(inst2.estimate('obj'))

#'''

# Example submarine model

position = vectort([boundedrealt(0., 30.), boundedrealt(0., 30.), boundedrealt(0., 30.)])

pool = model(
  {
    'distance': function(
      {'x': position, 'y': position}, 
      pow(pow(var('x')[0] - var('y')[0], lit(realr(2))) + pow(var('x')[1] - var('y')[1], lit(realr(2))) + pow(var('x')[2] - var('y')[2], lit(realr(2))), lit(realr(0.5))).raw
    ),
    'heading': function(
      {'x': position, 'y': position},
      atan2(var('y')[1] - var('x')[1], var('y')[0] - var('x')[0]).raw
    ),
    'id': function(
      {'x': position},
      var('x').raw
    )
  },
  {
    'sub': object(position, 0.9),
    'torpedoes': object(position, None)

  },
  300
)

inst = Instance('pool', model = pool)

print(inst.observe('id', {'x': pobject('sub')}, vectorr([realr(5), realr(5), realr(5)]), 0.5))
print(inst.observe('id', {'x': pobject('torpedoes')}, vectorr([realr(2), realr(2), realr(2)]), 1.0))

print(inst.estimate('sub'))
print(inst.estimate('torpedoes'))

print(inst.observe('distance', {'x': pobject('sub'), 'y': pobject('torpedoes')}, realr(9), 0.1))

print(inst.estimate('sub'))
print(inst.estimate('torpedoes'))

# print(inst.observe('heading', {'x': pobject('sub'), 'y': pobject('torpedoes')}, realr(50 * math.pi / 180), 0.01))

# print(inst.estimate('sub'))
# print(inst.estimate('torpedoes'))

# '''
