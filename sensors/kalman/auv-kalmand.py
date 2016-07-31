#!/usr/bin/env python3
''' The daemon that runs the Python Kalman filters for velocity and heading. '''

import math
import time

from functools import reduce

import numpy as np

import shm

from auv_math.quat import Quaternion
from auv_python_helpers.angles import abs_heading_sub_degrees
from conf.vehicle import sensors, VEHICLE
from settings import dt

rec_get_attr = lambda s: \
                 reduce(lambda acc, e: getattr(acc, e),
                        s.split('.'), shm)

# thruster_array allows access to thruster values
thrusters = ['port', 'starboard', 'sway_fore', 'sway_aft']

rate_var_imu = rec_get_attr(sensors["heading_rate"])
pitch_rate_var = rec_get_attr(sensors["pitch_rate"])
roll_rate_var = rec_get_attr(sensors["roll_rate"])

shm_quat = rec_get_attr(sensors["quaternion"])

vel_getters = {}
dvl_velocity = False
for vel_var in ["velx", "vely", "velz"]:
  if vel_var in sensors:
    vel_getters[vel_var] = rec_get_attr(sensors[vel_var])

    # XXX Fragile.
    dvl_velocity = True

  else:
    vel_getters[vel_var] = rec_get_attr("kalman." + vel_var)

depth_in = rec_get_attr(sensors["depth"])
depth_offset = rec_get_attr(sensors["depth_offset"])

# DVL Beam vars
beam_vars = [shm.dvl.low_amp_1,
        shm.dvl.low_amp_2,
        shm.dvl.low_amp_3,
        shm.dvl.low_amp_4,
        shm.dvl.low_correlation_1,
        shm.dvl.low_correlation_2,
        shm.dvl.low_correlation_3,
        shm.dvl.low_correlation_4]

wrench = shm.control_internal_wrench

#def CalibrateHeadingRate(var):
#    vals = []
#    for i in range(10):
#        vals.append(var.get())
#        time.sleep(0.02)
#    return sum(vals)/len(vals)
#rate_offset_imu = CalibrateHeadingRate(rate_var_imu)

from kalman_unscented import UnscentedKalmanFilter

def fx(x, dt):
    q_initial, ang_vel = Quaternion(q=x[:4], unit=False), x[4:]
    # I don't think the below is correct.
    # Because HPR != axis of angular velocity
    # TODO XXX FIX
    disp_quat = Quaternion(hpr=([math.degrees(vel * dt) for vel in ang_vel]))
    q_final = q_initial * disp_quat
    x[0] = q_final[0]
    x[1] = q_final[1]
    x[2] = q_final[2]
    x[3] = q_final[3]
    return x

def hx(x):
    return x

def get_orientation(quat_group):
    q_offset = Quaternion(hpr=(shm.kalman_settings.heading_offset.get(), 0, 0))
    quat = Quaternion(q=[quat_group.q0, quat_group.q1, quat_group.q2, quat_group.q3])
    return q_offset * quat

quat_group = shm_quat.get()
orientation_filter = UnscentedKalmanFilter(7, fx, 7, hx, dt, .1)
quat = get_orientation(quat_group).q
orientation_filter.x_hat = np.array([quat[0], quat[1], quat[2], quat[3], 0, 0, 0])
orientation_filter.P *= .5
orientation_filter.R = np.array([[90, 0, 0, 0, 0, 0, 0],
                                 [0, 90, 0, 0, 0, 0, 0],
                                 [0, 0, 90, 0, 0, 0, 0],
                                 [0, 0, 0, 90, 0, 0, 0],
                                 [0, 0, 0, 0, 1.5, 0, 0],
                                 [0, 0, 0, 0, 0, 1.7, 0],
                                 [0, 0, 0, 0, 0, 0, 1.5]])

def convert_dvl_velocities(sub_quat, dvl_vel):
    # TODO This transform should maybe be in a configuration file.
    # Or perhaps we should configure the DVL to do it for us.
    vel_body_frame = Quaternion(hpr=(0, 0, 180)) * dvl_vel
    ypr = sub_quat.hpr()
    vel_spitz_frame = (Quaternion(hpr=(ypr[0]%360, 0, 0)).conjugate() * sub_quat) * vel_body_frame
    return vel_spitz_frame

def get_velocity(sub_quat, depth, last_depth):
    vel = np.array([vel_getters[vel_var].get() for vel_var in \
                    ["velx", "vely", "velz"]])
    if dvl_velocity:
        vel = convert_dvl_velocities(sub_quat, vel)
    else:
        vel[2] = (depth - last_depth) / dt

    return vel

def get_depth():
    return depth_in.get() - depth_offset.get()

sub_quat = Quaternion(q=orientation_filter.x_hat[:4])
x_vel, y_vel, z_vel = get_velocity(sub_quat, 0, 0)

from kalman_position import PositionFilter
depth = get_depth()
kalman_xHat = np.array([[ x_vel, 0, y_vel, 0, 0, 0, depth, 0]]).reshape(8, 1)
# Pass in ftarray, shared memory handle to controller
kalman_position = PositionFilter(kalman_xHat)

start = time.time()
iteration = 0
while True:
    # TODO Should we wait on gx4 group write?
    while iteration*dt < time.time() - start:
        # Avoid timing errors due to time jumps on startup.
        if time.time() - start - iteration*dt > 60:
            start = time.time()
            iteration = 0

        yaw_rate_kal = math.radians(rate_var_imu.get())
        pitch_rate_kal = math.radians(pitch_rate_var.get())
        roll_rate_kal = math.radians(roll_rate_var.get())

        # Bugs arise due to quaternion aliasing, so we choose the quaternion
        # closest to the actual state
        quat_group = shm_quat.get()
        actual_quat = get_orientation(quat_group).q
        negated_quat = [-i for i in actual_quat]
        kalman_quat = orientation_filter.x_hat[:4]

        actual_delta = [kalman_quat[i] - actual_quat[i] for i in range(4)]
        negated_delta = [kalman_quat[i] - negated_quat[i] for i in range(4)]

        quat_in = actual_quat
        if np.linalg.norm(actual_delta) > np.linalg.norm(negated_delta):
            quat_in = negated_quat

        orientation_filter.predict()
        orientation_filter.update(list(quat_in) + [yaw_rate_kal, pitch_rate_kal, roll_rate_kal])

        # [q0, q1, q2, q3, yawrate, pitchrate, rollrate]
        data = orientation_filter.x_hat
        sub_quat = Quaternion(q=data[:4])
        ypr = sub_quat.hpr()

        outputs = shm.kalman.get()
        keys = ['q0', 'q1', 'q2', 'q3', 'heading_rate', 'pitch_rate', 'roll_rate']
        output = dict(zip(keys, data))
        outputs.update(**output)
        outputs.heading_rate = math.degrees(outputs.heading_rate)
        outputs.pitch_rate = math.degrees(outputs.pitch_rate)
        outputs.roll_rate = math.degrees(outputs.roll_rate)
        outputs.update(**{'heading': ypr[0] % 360,
                          'pitch':   ypr[1],
                          'roll':    ypr[2]})

        last_depth = depth
        depth = get_depth()

        x_vel, y_vel, z_vel = get_velocity(sub_quat, depth, last_depth)

        # Compensate for gravitational acceleration
        #grav_x = math.sin( math.radians(outputs.pitch) )*9.8 # XXX: CHRIS DOES NOT LIKE (small angle approx??)
        #grav_y = -math.sin( math.radians(outputs.roll) )*9.8
        #gx4_grav_y = np.tan(math.radians(outputs.pitch))*np.sqrt(shm.gx4.accelx.get()**2 + shm.gx4.accelz.get()**2)
        #gx4_grav_x = -1*np.tan(math.radians(outputs.roll))*shm.gx4.accelz.get()
        #him_grav_y = np.tan(math.radians(outputs.pitch))*np.sqrt(shm.him.x_accel.get()**2 + shm.him.z_accel.get()**2)
        #him_grav_x = -1*np.tan(math.radians(outputs.roll))*shm.him.z_accel.get()
        #x_acc = x_acc - grav_x
        #y_acc = y_acc - grav_y
        x_acc, y_acc = [0, 0] # temporary


        #Check whether the DVL beams are good
        beams_good = sum( [not var.get() for var in beam_vars] ) >= 2

        #beams_good = all( [not var.get() for var in beam_vars] )
        #And if not, disable them
        if not beams_good:
            active_measurements = np.array([0,1,0,1,1]).reshape((5,1))
        else:
            active_measurements = None

        # XXX Experimental.
        #active_measurements = np.array([1,0,1,0,1]).reshape((5,1))

        soft_kill = shm.switches.soft_kill.get()

        curr_thrusters = dict((t,(1-soft_kill)*shm.motor_desires.__getattribute__(t).get()) for t in thrusters)
        u = np.array((wrench.f_x.get(), wrench.f_y.get(), \
                      wrench.f_z.get(), wrench.t_x.get(), \
                      wrench.t_y.get(), wrench.t_z.get()))

        ## Update
        
        outputs.update(**kalman_position.update(outputs.heading, x_vel, x_acc,
                                                y_vel, y_acc, depth, u,
                                                active_measurements,
                                                curr_thrusters,
                                                outputs.pitch, outputs.roll))

        outputs.velz = z_vel
       
        # This really shouldn't be necessary when kalman has a u term (which it does)
        if not beams_good and VEHICLE is "thor":
            outputs.velx = 0
            outputs.vely = 0

        ## Write outputs as group, notify only once
        shm.kalman.set(outputs)

        iteration += 1

    time.sleep(dt/5.)

#@ kalman.heading.updating = shm.kalman.heading.get() != delayed(0.5, 'shm.kalman.heading.get()')
#@ kalman.heading.valid = 0 <= shm.kalman.heading.get() < 360
#@ kalman.velx.updating = shm.kalman.velx.get() != delayed(0.5, 'shm.kalman.velx.get()')
