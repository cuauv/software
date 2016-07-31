#!/usr/bin/env python3.4

import sys; sys.path.append('libs/python')

from aslam.core import *
from aslam.model import *
from aslam.function import *
from aslam.parameter import *
from aslam.type import *
from aslam.repr import *
from aslam.object import *

# import math, shm

import math, random

# Presume accelerometer has quadratic response.

gx4_coeff = vectort([boundedrealt(-5., 5.), boundedrealt(0.0, 3.0), boundedrealt(-5., 5.)])

sensors = model(
  {
    'gx4_response': function(
      {'coeff': gx4_coeff, 'act': boundedrealt(-5., 5.)},
      ( (var('coeff')[0] * pow(var('act'), lit(realr(2)))) + (var('coeff')[1] * var('act')) + var('coeff')[2] ).raw,
      )
  },
  {
    'gx4': object(gx4_coeff, None)
  },
  300
)

inst = Instance('sensors', sensors)

print(inst.estimate('gx4'))

act = [random.random(), random.random(), random.random()]

print('Actual: {0}'.format(act))

f = lambda r: (act[0] * pow(r, 2)) + (act[1] * r) + act[2]

for _ in range(20):
  t = random.random()
  print(inst.observe('gx4_response', {'coeff': pobject('gx4'), 'act': pactual(realr(t))}, realr(f(t)), 0.1))

print(inst.estimate('gx4'))
