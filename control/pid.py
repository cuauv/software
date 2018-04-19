import operator
import math
import time

import numpy as np

from auv_math.quat import Quaternion
from auv_python_helpers.angles import heading_sub_degrees

from shm import kalman, dvl, desires, settings_control, \
                control_internal_depth, control_internal_heading, \
                control_internal_pitch, control_internal_velx, \
                control_internal_vely, control_internal_roll, \
                settings_depth, settings_heading, settings_pitch, \
                settings_velx, settings_vely, settings_roll, settings_quat, \
                control_locked

from conf.vehicle import sensors

class PID:
  """
    This is meant to be generic. The value 0 should never be used.
  """
  def __init__(self, P, I, D, diff_func=operator.sub, speed=1):
    self.P = P
    self.I = I
    self.D = D
    self.diff_func = diff_func
    self.speed = speed
    self.rD = None
    self.reset()

  def reset(self):
    self.integral = None
    self.last_time = 0
    self.last_value = None
    self.locked = False
    self.out_P = None
    self.out_I = None
    self.out_D = None

  def tick(self, value, desired, rate_var=None):
    # TODO shoud we pass in dt as an argument? (DEPENDENCY INJECTION!)
    now = time.time()
    dt = (now - self.last_time) * self.speed
    self.last_time = now

    error = self.diff_func(desired, value)

    # Ignore any pauses in the controller.
    if dt < 5:
      if self.integral is None:
        self.integral = error * dt
      else:
        self.integral += error * dt

    # TODO When else should we fiddle with integral?
    # This generic PID loop erased many cased deemed unnecessary...
    if self.rD is not None and abs(error) > self.rD:
      self.integral = None
      self.locked = True
    else:
      self.locked = False

    self.out_P = self.P * error
    output = self.out_P

    if self.integral is not None:
      self.out_I = self.I * self.integral
      output += self.out_I
    else:
      self.out_I = None

    # Avoid derivative spike on startup.
    if self.last_value is not None:
      if rate_var is not None:
        deriv = -rate_var
      else:
        deriv = self.diff_func(-value, -self.last_value) / dt

      self.out_D = self.D * deriv
      output += self.out_D
    else:
      self.out_D = None

    self.last_value = value

    return output


class DynamicPID(PID):
    """A PID loop which can update the PID values on each tick."""

    def __init__(self, P:float = 1, I:float = 0, D:float = 0, diff_func=operator.sub, speed=1):
        super().__init__(P, I, D, diff_func, speed)

    def tick(self, value: float, desired: float, p: float = None, d: float = None, i: float = None):
        if p is not None:
            self.P = p

        if d is not None:
            self.D = d

        if i is not None:
            self.I = i

        return super().tick(value, desired)


class ShmPID:
  def __init__(self, gain_group, value, desire, diff_func, out_group, on, locked, rate_var=None, speed=1):
    self.gain_group = gain_group
    self.value = value
    self.desire = desire
    self.out_group = out_group
    self.on = on
    self.locked = locked
    self.rate_var = rate_var

    self.pid = PID(0, 0, 0, diff_func, speed)
    self.update_gains()

  def update_gains(self):
    self.pid.P = self.gain_group.kP.get()
    self.pid.I = self.gain_group.kI.get()
    self.pid.D = self.gain_group.kD.get()
    self.pid.rD = self.gain_group.rD.get()

  def reset(self):
    self.pid.reset()
    self.out_group.integral.set(0)
    self.out_group.out.set(0)

  def tick(self):
    self.update_gains()

    if self.on.get():
      rate = self.rate_var() if self.rate_var is not None else None
      out = self.pid.tick(self.value.get(), self.desire.get(),
                          rate_var=rate)
    else:
      out = 0

    self.out_group.out.set(out)

    if self.pid.integral is None:
      integral = 0
    else:
      integral = self.pid.integral

    self.out_group.integral.set(integral)

    # Output helpful debugging data.
    out_P = 0 if self.pid.out_P is None else self.pid.out_P
    out_I = 0 if self.pid.out_I is None else self.pid.out_I
    out_D = 0 if self.pid.out_D is None else self.pid.out_D
    self.out_group.out_P.set(out_P)
    self.out_group.out_I.set(out_I)
    self.out_group.out_D.set(out_D)

    self.locked.set(self.pid.locked)

class PIDLoop:
    """ Class for updating PID values """

    def __init__(self, speed=1.0):
        """
        Initializes the vehicle PID controller.

        Arguments:
        speed -- a float multiplier to apply to the real time passed to obtain
                 the amount of simulated time passed. For example, if speed is
                 2.0 and 0.1 seconds have passed since the last step, dt will
                 be calculated as if 0.2 seconds passed.
                 This does not affect the time returned by step().
        """

        # Only use the z_vel variable if the vehicle actually has a way of
        # measuring z velocity directly
        if "velz" in sensors:
            depth_rate = lambda: kalman.velz.get()
        else:
            depth_rate = None

        velx_pid = ShmPID(settings_velx, kalman.velx, desires.speed,
                          operator.sub, control_internal_velx,
                          settings_control.velx_active,
                          control_locked.velx, speed=speed)
        vely_pid = ShmPID(settings_vely, kalman.vely, desires.sway_speed,
                          operator.sub, control_internal_vely,
                          settings_control.vely_active, control_locked.vely,
                          speed=speed)
        depth_pid = ShmPID(settings_depth, kalman.depth,
                           control_internal_depth.desire, operator.sub,
                           control_internal_depth,
                           settings_control.depth_active, control_locked.depth,
                           rate_var=depth_rate, speed=speed)

        heading_pid = ShmPID(settings_heading, kalman.heading, desires.heading,
                             heading_sub_degrees, control_internal_heading,
                             settings_control.heading_active,
                             control_locked.heading,
                             rate_var=lambda: self.heading_rate, speed=speed)
        pitch_pid = ShmPID(settings_pitch, kalman.pitch, desires.pitch,
                           heading_sub_degrees, control_internal_pitch,
                           settings_control.pitch_active, control_locked.pitch,
                           rate_var=lambda: self.pitch_rate, speed=speed)
        roll_pid = ShmPID(settings_roll, kalman.roll, desires.roll,
                          heading_sub_degrees, control_internal_roll,
                          settings_control.roll_active, control_locked.roll,
                          rate_var=lambda: self.roll_rate, speed=speed)

        self.pids = [velx_pid, vely_pid, depth_pid, heading_pid, pitch_pid, roll_pid]

        self.clean()
        self.last_q_error = 0
        self.last_quat_time = time.time()
        self.speed = speed

    def clean(self):
        """ Clean the controller state; init all variables """
        self.last_time = time.time()
        [pid.reset() for pid in self.pids]
        control_internal_depth.desire.set(desires.depth.get())

        # Added by Christopher
        self.integral = 0

    def quat_pid(self):
        # TODO Figure out how to formalize this and use generic PID class.
        g = kalman.get()
        d = desires.get()

        a = Quaternion(q=[g.q0, g.q1, g.q2, g.q3])
        b = Quaternion(hpr=(d.heading, d.pitch, d.roll))

        current_time = time.time()
        dt = (current_time - self.last_quat_time) * self.speed
        self.last_quat_time = current_time

        q_diff = b * a.conjugate()
        if abs(abs(q_diff[0]) - 1) < 1e-15:
            self.last_q_error = np.array((0, 0, 0))
            return np.array((0, 0, 0))

        ax = q_diff.axis()
        ang = q_diff.angle()

        # Ensure we are taking the shortest path around.
        if ang > math.pi:
          ax = -ax
          ang = 2*math.pi - ang

        ang_accel = ang * ax * settings_quat.kP.get()

        diff = (ang * ax - self.last_q_error) / dt
        self.last_q_error = ang * ax
        ang_accel += settings_quat.kD.get() * diff

        # added by Christopher
        #self.integral = self.integral + ax * dt
        #ax += settings_quat.kI.get() * self.integral

        return ang_accel

    def update_angular_rates(self, tm):
        # Convert from body frame to spitz space (heading-adjusted world space).
        ang_rates_sub = np.array((kalman.roll_rate.get(), kalman.pitch_rate.get(), kalman.heading_rate.get()))
        ang_rates_spitz = tm.spitz_to_sub_quat.conjugate() * ang_rates_sub
        self.roll_rate = ang_rates_sub[0]
        self.pitch_rate = ang_rates_spitz[1]
        self.heading_rate = ang_rates_spitz[2]

    def step(self, tm):
        self.update_angular_rates(tm)
        for pid in self.pids:
          pid.tick()

        now_time = time.time()
        dt = (now_time - self.last_time) * self.speed
        self.last_time = now_time

        ### Depth ramping
        # TODO: Experiment with ramping other controllers
        if control_internal_depth.desire.get() != desires.depth.get():
            diff = desires.depth.get() - control_internal_depth.desire.get()
            step = dt * settings_depth.ramp_speed.get()

            if step > abs(diff) or abs(diff) > 10: #TODO: Work on this too-large-change feature
                control_internal_depth.desire.set(desires.depth.get())
            else:
                d = step if diff > 0 else -step
                control_internal_depth.desire.set(control_internal_depth.desire.get() + d)
