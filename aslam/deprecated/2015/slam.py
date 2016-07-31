'''

Hacks.

'''

'''
initial = {
  'sub_delta': [0., 0., 0.],
  'gate': [1., 1., 1.],
  'red_buoy': [0., 0., 0.],
  'green_buoy': [0., 0., 0.],
  'yellow_buoy': [0., 0., 0.],
  'wire': [0., 0., 0.],
  'bins': [0., 0., 0.],
  'torpedoes': [0., 0., 0.],
  'octagon_1': [0., 0., 0.],
  'octagon_2': [0., 0., 0.],
  'train_track': [0., 0., 0.]
}
'''

import json, shm, math

# from model import *

# from aslam.type import *
# from aslam.repr import *
# from aslam.parameter import *

def h2s(h):
  if h > 180: h = h - 360
  h = math.radians(h)
  return h

class Position:
  @staticmethod
  def ZERO(): return Position(0, 0, 0)

  @staticmethod
  def UP(m): return Position(0, 0, -m)
  
  @staticmethod
  def DOWN(m): return Position(0, 0, m)

  @staticmethod
  def RAY(heading, distance):
    heading = h2s(heading)
    # heading = math.radians(heading)
    return Position(math.cos(heading) * distance, math.sin(heading) * distance, 0)

  def __init__(self, n, e, d):
    self.n, self.e, self.d = n, e, d
  
  def __sub__(self, other):
    return Position(self.n - other.n, self.e - other.e, self.d - other.d)

  def __add__(self, other):
    return Position(self.n + other.n, self.e + other.e, self.d + other.d)

  def __str__(self): return repr(self)
  def __repr__(self): return 'Position<n={0},e={1},d={2}>'.format(round(self.n, 3), round(self.e, 3), round(self.d, 3))
  
  def norm(self):
    return (self.n ** 2. + self.e ** 2. + self.d ** 2.) ** (1./2)

  def heading(self):
    return math.degrees(math.atan2(self.e, self.n))

  '''
  def r(self):
    return vectorr([realr(x) for x in [self.n, self.e, self.d]])
  '''

class TrackedObject:
  def __init__(self, name, pos):
    self.name, self.pos = name, pos

  def position(self):
    return self.pos
    # return Position(*[x['realr'] for x in slam.estimate(self.name)['estimated']['vectorr']])
  
  def force_heading(self, from_pos, heading):
    distance = (self.position() - from_pos).norm()
    prev_d = self.pos.d
    self.pos = from_pos + Position.RAY(heading, distance)
    self.pos.d = prev_d

class Sub(TrackedObject):
  def __init__(self):
    self.name = 'sub_delta'
  
  def zero(self):
    self.n_offset, self.e_offset, self.h_offset = shm.kalman.north.get(), shm.kalman.east.get(), shm.kalman.heading.get()

  def raw_n(self):
    return shm.kalman.north.get() - self.n_offset

  def raw_e(self):
    return shm.kalman.east.get() - self.e_offset
    
  def raw_d(self):
    return shm.kalman.depth.get()

  def raw_h(self):
    return shm.kalman.heading.get() - self.h_offset

  def raw(self):
    return Position(self.raw_n(), self.raw_e(), self.raw_d())

  def est(self):
    return Position.ZERO()
    # return Position(*[x['realr'] for x in slam.estimate(self.name)['estimated']['vectorr']])

  def position(self):
    return self.raw() + self.est()

  '''
  def delta(self, obj):
    # Get relative delta.
    pos = obj.position()
    rel = self.position()
    delta = pos - rel
    return delta.r()
   
  def headingTo(self, obj, heading):
    heading = h2s(heading)
    print(slam.observe('heading', {'x': pobject(self.name), 'y': pactual(self.delta(obj))}, realr(heading), 0.5))
    print(slam.observe('heading', {'x': pactual(self.position().r()), 'y': pobject(obj.name)}, realr(heading), 0.5))

  def distanceTo(self, obj, distance):
    print(slam.observe('distance', {'x': pobject(self.name), 'y': pactual(self.delta(obj))}, realr(distance), 0.2))
    print(slam.observe('distance', {'x': pactual(self.position().r()), 'y': pobject(obj.name)}, realr(distance), 0.2))
  '''

# Point sub at course north and zero.

world = {}

def initialize(f):
  initial = json.loads(open(f).read())
  for k, v in initial.items():
    world[k] = TrackedObject(k, Position(v['n'], v['e'], v['d']))

world['sub'] = Sub()

  # for k, v in initial.items():
    #slam.observe('id', {'x': pobject(k)}, vectorr([realr(x) for x in v]), 0.1)
  # world['sub'].distanceTo(world['gate'], 0.0) # some bug, does nothing
