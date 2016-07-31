import shm
import time
import os
import math
import random
import numpy as n
import auvlog.client

import conf.vehicle as vehicle
import conf.auv_locale as locale
from auv_python_helpers.angles import *

log = auvlog.client.log.aslam.py

_get_shm_out  = lambda n: getattr(shm, 'aslam_' + n)

class Wrapper:
  pass

def to_hpd(vec):
  return n.array([math.atan2(vec[1], vec[0]), math.atan2(vec[2], (vec[1] ** 2. + vec[0] ** 2.) ** 0.5), n.linalg.norm(vec)])

def gaussian(x, mu, variance):
  return math.exp( - ((x - mu) ** 2.) / (2 * variance))

class Object:
  __slots__ = ['name', 'group_out', 'components', 'offset', '_initial']

  def __init__(self, name, initial, components = {}):
    self.name = name
    self.components = Wrapper()
    for name, offset in components.items():
      setattr(self.components, name, Component(name, offset, self))
    self.group_out = _get_shm_out(self.name)
    self._initial = n.array(initial)
    self.offset = n.array([0., 0., 0.])

  def position(self):
    north, east, depth = self.group_out.north.get(), self.group_out.east.get(), self.group_out.depth.get()
    return n.array([north, east, depth])

  def initial(self):
    return self._initial

  def gin(self):
    return self.group_in

  def gout(self):
    return self.group_out

  def uncertainty(self):
    north, east, depth = self.group_out.north_uncertainty.get(), self.group_out.east_uncertainty.get(), self.group_out.depth_uncertainty.get()
    return n.array([north, east, depth])

class Component(Object):
  __slots__ = ['name', 'offset', 'parent', 'group_in', 'group_out']

  def __init__(self, name, offset, parent):
    self.name, self.offset, self.parent = name, offset, parent

  def gin(self):
    return self.parent.group_in

  def gout(self):
    return self.parent.group_out

  def position(self):
    return self.parent.position() + self.offset

  def initial(self):
    return self.parent.initial() + self.offset

class Submarine(Object):
  __slots__ = ['name', 'group', 'offset', 'components']

  def __init__(self, components):
    self.name = os.environ.get('CUAUV_SUBMARINE', 'sim')
    self.group = shm.aslam_sub
    self.components = Wrapper()
    for name, offset in components.items():
      setattr(self.components, name, Component(name, offset, self))
    self.offset = n.array([0., 0., 0.])

  def position(self):  
    north, east, depth = shm.kalman.north.get(), shm.kalman.east.get(), shm.kalman.depth.get()
    return n.array([north, east, depth])

  def orientation(self):
    heading, pitch, roll = self.group.heading.get(), self.group.pitch.get(), self.group.roll.get()
    return n.array([heading, pitch, roll])

  def move_to(self, position):
    shm.navigation_desires.north.set(position[0])
    shm.navigation_desires.east.set(position[1])
    shm.navigation_desires.depth.set(position[2])

  def uncertainty(self):
    north, east, depth = self.group.north_uncertainty.get(), self.group.east_uncertainty.get(), self.group.depth_uncertainty.get()
    return n.array([north, east, depth])

sub = Submarine(vehicle.components)
world = Wrapper()

for obj in locale.objects:
  setattr(world, obj['name'], Object(obj['name'], obj['initial_position'], obj.get('components', {})))
