import sys

sys.path.append('/home/software/trunk/aslam/v3/libs/python')

from aslam.core import *
from aslam.model import *
from aslam.function import *
from aslam.parameter import *
from aslam.type import *
from aslam.repr import *
from aslam.object import *

import math, shm, json

position = vectort([boundedrealt(-20., 20.), boundedrealt(-20., 20.), boundedrealt(-20., 20.)])

transdec = model(
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
    ),
    'delta': function(
      {'o': position, 'n': position},
      vece([ (var('n')[0] - var('o')[0]).raw, (var('n')[1] - var('o')[1]).raw, (var('n')[2] - var('o')[2]).raw ])
    ),
    'depth': function(
      {'x': position},
      var('x')[2].raw
    )
  },
  {
    'sub_delta': object(position, None),
    'start': object(position, None),
    'gate_pipe': object(position, None),
    'gate': object(position, None),
    'red_buoy': object(position, None),
    'green_buoy': object(position, None),
    'yellow_buoy': object(position, None),
    'buoy_pipe': object(position, None),
    'wire': object(position, None),
    'bins': object(position, None),
    'torpedoes': object(position, None),
    'octagon_1': object(position, None),
    'octagon_2': object(position, None),
    'train_track': object(position, None)
  },
  200
)

slam = Instance('transdec', model = transdec)
