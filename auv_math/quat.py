'''
Quaternion based methods and objects
'''

import numpy as np
from math import sin, cos, asin, atan2, degrees, radians, acos
import ctypes

from auv_python_helpers import load_library
quat_lib = load_library("libquat.so")

quat_t = ctypes.c_double * 4
vect_t = ctypes.c_double * 3
double_t = ctypes.c_double
ret_quat = quat_t()
ret_vect = vect_t()

def quat_from_axis_angle(axis, angle):
    """ Axis is normalized and angle is in radians in the range [-pi, pi] """
    v = axis * sin(angle/2.0)
    return Quaternion(q=[cos(angle/2.0), v[0], v[1], v[2]])

class Quaternion(object):
    """ A Quaternion """
    def __init__(self, q=None, hpr=None, unit=True):
        if q is not None:
            self.q = np.array(q)

        else:
            quat_lib.hpr_to_quat(double_t(hpr[0]), double_t(hpr[1]),
                                 double_t(hpr[2]), ret_quat)
            self.q = np.array((ret_quat))

        if unit:
            self.normalize()

        self.imag = self.q[1:]

    def conjugate(self):
        return Quaternion(q=[self.q[0], -self.q[1], -self.q[2], -self.q[3]])

    def norm(self):
        return np.linalg.norm(self.q)

    def normalize(self):
        norm = self.norm()
        if norm < 1e-20:
            raise Exception("Quaternion of norm 0!")

        self.q = self.q / norm
        self.imag = self.q[1:]

    def __mul__(self, other):
        if isinstance(other, Quaternion):
            quat_lib.quat_quat_mult(quat_t(*self.q), quat_t(*other.q), ret_quat)
            return Quaternion(q=ret_quat, unit=False)

        else:
            quat_lib.quat_vector_mult(quat_t(*self.q), vect_t(*other), ret_vect)
            return np.array(ret_vect)

    def __getitem__(self, index):
        return self.q[index]

    def __repr__(self):
        return "Quaternion: (%f, %f, %f, %f)" % (self.q[0], self.q[1],
                                                 self.q[2], self.q[3])

    def __iter__(self):
        for q in self.q:
            yield q

    def axis(self):
        return self.imag / sin(self.angle() / 2.0)

    def angle(self):
        """ Returns angle in the range [0, 2pi] """
        return acos(self.q[0]) * 2

    def roll(self):
        q0, q1, q2, q3 = self.q
        return degrees(atan2(2 * (q0*q1 + q2*q3), 1 - 2 * (q1**2 + q2**2)))

    def pitch(self):
        q0, q1, q2, q3 = self.q
        term = 2 * (q0*q2 - q1*q3)
        if term > 1.0:
            term = 1.0
        elif term < -1.0:
            term = -1.0

        return degrees(asin(term))

    def heading(self):
        q0, q1, q2, q3 = self.q
        return degrees(atan2(2 * (q0*q3 + q1*q2), 1 - 2 * (q2**2 + q3**2)))

    def hpr(self):
        """
            Returns heading, pitch, and roll as a 3-tuple in degrees.
        """
        return [self.heading(), self.pitch(), self.roll()]

    def matrix(self):
        qw, qx, qy, qz = self.q
        ret = np.empty((3, 3))
        ret[0][0] = 1 - 2*qy**2 - 2*qz**2
        ret[0][1] = 2*qx*qy - 2*qz*qw
        ret[0][2] = 2*qx*qz + 2*qy*qw
        ret[1][0] = 2*qx*qy + 2*qz*qw
        ret[1][1] = 1 - 2*qx**2 - 2*qz**2
        ret[1][2] = 2*qy*qz - 2*qx*qw
        ret[2][0] = 2*qx*qz - 2*qy*qw
        ret[2][1] = 2*qy*qz + 2*qx*qw
        ret[2][2] = 1 - 2*qx**2 - 2*qy**2

        return ret
