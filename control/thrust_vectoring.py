import math
import numpy as np

FULL_RANGE = 1024

class VectoringThrusterData(object):
  def __init__(self, position, axis, start, angle, offset, _reversed = False):
    vector_axis = np.array(axis)
    vector_1 = np.array(start)
    self.vector_axis = vector_axis / np.linalg.norm(vector_axis)
    self.force_1 = vector_1 / np.linalg.norm(vector_1)

    # Enforce orthogonality.
    assert self.force_1.dot(self.vector_axis) < 1e-8
    # assert 0 < angle < 360
    self.angle = math.radians(angle)

    self.force_2 = np.cross(self.vector_axis, self.force_1)

    self.torque_1 = np.cross(np.array(position), self.force_1)
    self.torque_2 = np.cross(np.array(position), self.force_2)

    self.torque_weight = 1

    self.offset = offset
    self._reversed = _reversed

    # 1024 = 2pi rad
    # self.rads_per_value = self.angle / (1024 / 2 / math.pi)
    self.rads_per_value = 2 * math.pi / 1024

  def angle_from_value(self, value):
    return ((value - self.offset) % FULL_RANGE) * self.rads_per_value

  def value_from_angle(self, angle):
    return (int(round(angle / self.rads_per_value)) + self.offset) % FULL_RANGE

  def get_angle(self, desired_output_s):
    force = desired_output_s[:3]
    torque = desired_output_s[3:]

    # Our thrust data, self.force_1, self.torque_1, etc. is in sub space,
    # The angle will be the same if we convert the incoming forces and torques
    # that are now in world space into sub space.
    opt_angle = math.atan2(self.torque_2.dot(torque) * self.torque_weight + \
                             self.force_2.dot(force), \
                           self.torque_1.dot(torque) * self.torque_weight + \
                             self.force_1.dot(force))

    # Currently vector motor offset by pi/2
    if opt_angle < 0:
      opt_angle += 2*math.pi

    # Thrusters work both ways.
    # TODO is it worth favoring positive thrust over negative thrust?
    if opt_angle > math.pi:
      opt_angle -= math.pi

    # Find the angle that most closely matches the calculated.
    # Note that if the angle of thrust vectoring is more than half of a
    # circle i.e. more than pi, all the below checks will fail so this should
    # work in all cases.

    # There is a legitmate potential here for oscillations when desires
    # are equally between both the negative and positive thrust cones.
    thresh = 0.5*(math.pi + self.angle)
    if opt_angle >= thresh:
      opt_angle = 0
    elif self.angle < opt_angle < thresh:
      opt_angle = self.angle

    return opt_angle
