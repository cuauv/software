import shm
import numpy as n

from aslam.py.common import *
from aslam.py.core import *

@observation
def observe_buoy_a():
  return from_forward_vision(shm.a_buoy_results, world.buoy_a, 0.2)

@observation
def observe_buoy_b():
  return from_forward_vision(shm.b_buoy_results, world.buoy_b, 0.2)

@observation
def observe_buoy_c():
  return from_forward_vision(shm.c_buoy_results, world.buoy_c, 0.2)

@observation
def observe_buoy_d():
  return from_forward_vision(shm.d_buoy_results, world.buoy_d, 0.2)
