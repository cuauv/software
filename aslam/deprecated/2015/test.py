#!/usr/bin/env python3.4

import random

from model import *
from aslam.type import *
from aslam.repr import *

slam.observe('id', {'x': pobject('sub_delta')}, vectorr([realr(0.), realr(0.), realr(0.)]), 0.1)

real = [0., 0., 0.]

while 1:
  delta = [random.random(), random.random(), random.random()]
  prev = [x['realr'] for x in slam.estimate('sub_delta')['estimated']['vectorr']]
  # new = [prev[0] + delta[0], prev[1] + delta[1], prev[2] + delta[2]]
  new = prev
  real = [real[0] + delta[0], real[1] + delta[1], real[2] + delta[2]]
  slam.observe('id', {'x': pobject('sub_delta')}, vectorr([realr(x) for x in new]), 0.1)
  post = [x['realr'] for x in slam.estimate('sub_delta')['estimated']['vectorr']]
  print('Delta: {0} Real: {1} EstPr: {2}, EstPos: {3}'.format(delta, real, prev, post))
