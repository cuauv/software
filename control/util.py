"""
Centralized location for useful control associated functions and variables
2013
"""
from control.thrusters import thrusters, desires


def set_all_motors_from_seq(pwms, got_thrusters):
    g = desires.group()
    for i, motor in enumerate(got_thrusters):
        motor.update_shm_group(g, pwms[i])
    desires.set(g)


def set_all_motors(pwm):
    got_thrusters = thrusters()
    set_all_motors_from_seq([pwm] * len(got_thrusters), got_thrusters)


def zero_motors():
    set_all_motors(0)


def set_shm_wrench(shm_group, wrench):
    g = shm_group.group()
    g.f_x = wrench[0]
    g.f_y = wrench[1]
    g.f_z = wrench[2]
    g.t_x = wrench[3]
    g.t_y = wrench[4]
    g.t_z = wrench[5]
    shm_group.set(g)


class DOFSet(object):
    """ Ensures consistent ordering of our 6 DOFs """

    def __init__(self, l=None, f=0, s=0, d=0, r=0, p=0, y=0):
        if l is None:
            self.forward = f
            self.sway = s
            self.depth = d
            self.roll = r
            self.pitch = p
            self.yaw = y
        else:
            self.forward = l[self.forward_i]
            self.sway = l[self.sway_i]
            self.depth = l[self.depth_i]
            self.roll = l[self.roll_i]
            self.pitch = l[self.pitch_i]
            self.yaw = l[self.yaw_i]

    torque = [False, False, False, True, True, True]
    forward_i = 0
    sway_i = 1
    depth_i = 2
    roll_i = 3
    pitch_i = 4
    yaw_i = 5

    def __iter__(self):
        return iter([self.forward, self.sway, self.depth, \
                     self.roll, self.pitch, self.yaw])
