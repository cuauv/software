from collections import namedtuple

import math
import numpy as n

import aslam.py.core as aslam
from mission.framework.helpers import *

from auv_python_helpers.cameras import *
from auv_python_helpers.angles import *

all_observable_objects = []

def observation(func):
  all_observable_objects.append(func)

def from_forward_vision(group, obj, object_radius):
  results = group.get()
  if results.probability > 0:
    pixel_size    = get_camera_pixel_size(results)
    focal_length  = get_camera_focal_length(results)
    ctr_x, ctr_y  = get_camera_center(results)
    obj_x, obj_y  = results.center_x, results.center_y
    rad = results.radius

    calc_angle_subtended = calc_angle(rad + ctr_x, ctr_x, pixel_size, focal_length)
    calc_distance = object_radius / math.sin(calc_angle_subtended) 
    calc_heading = calc_angle(obj_x, ctr_x, pixel_size, focal_length)
    calc_pitch = -calc_angle(obj_y, ctr_y, pixel_size, focal_length)
    
    return aslam.Observation(aslam.sub, obj, n.array([calc_heading, calc_pitch, calc_distance]), n.array([0.5, 0.5, 2.0]) / results.probability / max(calc_distance, 1.0))
  return None
