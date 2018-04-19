import shm
import math
import numpy as n

REFRAC_INDEX_AIR = 1.0
REFRAC_INDEX_WATER = 1.33

def heading_diff(x, y):
    diff = (x - y) % (2 * math.pi)
    if diff > math.pi:
      diff -= 2 * math.pi
    return diff

def calc_angle_across_medium(angle_in_inside_medium, refrac_index_outside_medium, refrac_index_inside_medium):
    return math.asin( (refrac_index_inside_medium / refrac_index_outside_medium) * math.sin(angle_in_inside_medium) )

def calc_field_of_view(cam, dimension):
    """
    Calculates the field of view angle in radians of the camera given a
    dimension in mm.
    """
    angle_in_air    =  2 * math.atan((dimension / 2) / cam['focal_length_mm'])
    angle_in_water  = calc_angle_across_medium(angle_in_air, REFRAC_INDEX_WATER, REFRAC_INDEX_AIR)
    return angle_in_water

def calc_forward_angles(cam, coord):
    """
    Calculates the heading and pitch of the given (x, y) coordinate relative to
    given camera in radians.

    It is "as if" the sensor was smaller. See
    https://en.wikipedia.org/wiki/Angle_of_view for a nice picture.
    """
    pixel_size = cam['sensor_size_wh_mm'][0] / cam['width']
    diff_x = pixel_size * (coord[0] - cam['width'] / 2)
    diff_y = pixel_size * (cam['height'] / 2 - coord[1])
    heading = calc_field_of_view(cam, 2 * diff_x) / 2
    pitch = calc_field_of_view(cam, 2 * diff_y) / 2
    return (heading, pitch)

def calc_downward_angles(cam, coord):
    forward_heading, forward_pitch = calc_forward_angles(cam, coord)
    center_x, center_y = cam['width'] / 2, cam['height'] / 2
    heading = None
    if center_y == coord[1]:
        if coord[0] >= center_x:
            heading = 0
        else:
            heading = math.pi
    else:
        heading = math.atan((coord[0] - center_x) / (center_y - coord[1]))
    if coord[0] < center_x:
        heading += math.pi
    pitch = forward_pitch - math.pi / 2
    return (heading, pitch)

def offset_to_world(sub, offset):
  pos = n.array([sub.north, sub.east, sub.depth])
  pos[0] += (offset[0] * math.cos(sub.heading)) - (offset[1] * math.sin(sub.heading))
  pos[1] += (offset[0] * math.sin(sub.heading)) + (offset[1] * math.cos(sub.heading))
  pos[2] += offset[2]
  return pos

'''
Given a camera (from the submarine configuration), an x-y coordinate (origin at bottom left of image), and a distance, calculate the projected position of an object from an observation.

This can be used to reject observations that are clearly of objects out of the water or below the pool floor.
'''
def calc_projected_forward_position(cam, coord, distance):
  heading, pitch = calc_forward_angles(cam, coord)
  sub = shm.kalman.get()
  sub.heading = math.radians(sub.heading)
  sub.pitch = math.radians(sub.pitch)
  offset = cam['position']
  origin = offset_to_world(sub, offset)
  vec = n.array([math.cos(heading_diff(heading, -sub.heading)), math.sin(heading_diff(heading, -sub.heading)), math.sin(-heading_diff(pitch, -sub.pitch))])
  vec /= n.linalg.norm(vec)
  vec *= distance
  return origin + vec
