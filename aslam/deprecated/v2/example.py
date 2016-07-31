#!/usr/bin/env python2.7

import sys; sys.path.append('libs/python')
import random

from aslam.core import Client

from aslam.model import *
from aslam.observation import *
from aslam.atomicrequest import *

from aslam.repr import *
from aslam.type import *
from aslam.expression import *

aslam = Client('test')

# Rudimentary example: identity function.

identity = model(
  {'myfunc': [{'x': boundedreal(-5, 5)}, var('x')]},
  {'x': boundedreal(-5, 5)}
)

aslam.run(load(identity))

actual = lambda x: x

x = (random.random() * 10) - 5

print('Actual x: {0}.'.format(x))

aslam.run(observe(observation('myfunc', {'x': {'Left': 'x'}}, realr(actual(x)), 0.1)))

print('Estimated x: {0}.'.format(aslam.run(estimate('x'))))

# System of linear equations.

lineq = model(
  {'f1': [{'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)])}, add(ind(var('v'), 0), add(ind(var('v'), 1), ind(var('v'), 2)))], # x + y + z
   'f2': [{'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)])}, add(ind(var('v'), 0), sub(ind(var('v'), 1), ind(var('v'), 2)))], # x + y - z
   'f3': [{'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)])}, add(ind(var('v'), 0), sub(ind(var('v'), 2), ind(var('v'), 1)))]}, # x - y + z
  {'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)])}
)

aslam.run(load(lineq))

x, y, z = [(random.random() * 10) - 5 for _ in range(3)]

print('Actual x, y, z: {0}, {1}, {2}.'.format(x, y, z))

aslam.run(observe(observation('f1', {'v': {'Left': 'v'}}, realr(x + y + z), 0.1)))
aslam.run(observe(observation('f2', {'v': {'Left': 'v'}}, realr(x + y - z), 0.1)))
aslam.run(observe(observation('f3', {'v': {'Left': 'v'}}, realr(x - y + z), 0.1)))

res = aslam.run(estimate('v'))

print('Estimated x, y, z: {0}, {1}, {2}'.format(res[0], res[1], res[2]))

# Quadratic

quad = model(
  {'f1': [{'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)]), 'x': boundedreal(-5, 5)}, add(mul(ind(var('v'), 0), pow(var('x'), doublel(2))), add(mul(ind(var('v'), 1), var('x')), ind(var('v'), 2)))]},
  {'v': vector([boundedreal(-5, 5), boundedreal(-5, 5), boundedreal(-5, 5)])}
)

aslam.run(load(quad))

actual = lambda x: (a * x ** 2.) + (b * x) + c + (random.random() * 2.) - 1.

a, b, c, x1, x2, x3 = [(random.random() * 10) - 5 for _ in range(6)]

print('Actual a, b, c: {0}, {1}, {2}.'.format(a, b, c))

for _ in range(10):
  aslam.run(observe(observation('f1', {'v': {'Left': 'v'}, 'x': {'Right': realr(x1)}}, realr(actual(x1)), 0.1)))
  aslam.run(observe(observation('f1', {'v': {'Left': 'v'}, 'x': {'Right': realr(x2)}}, realr(actual(x2)), 0.1)))
  aslam.run(observe(observation('f1', {'v': {'Left': 'v'}, 'x': {'Right': realr(x3)}}, realr(actual(x3)), 0.1)))

res = aslam.run(estimate('v'))

print('Estimated a, b, c: {0}, {1}, {2}.'.format(res[0], res[1], res[2]))
