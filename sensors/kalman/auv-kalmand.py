#!/usr/bin/env python3
''' 
The daemon that runs Kalman filters for orientation and position.
'''

import math
import time
from functools import reduce
import numpy as np

import shm

from auv_math.quat import Quaternion
from auv_python_helpers.angles import abs_heading_sub_degrees
from conf.vehicle import sensors, dvl_present
from settings import dt

from kalman_unscented import UnscentedKalmanFilter
from kalman_position import PositionFilter

rec_get_attr = lambda s: \
                 reduce(lambda acc, e: getattr(acc, e),
                        s.split('.'), shm)

# Thruster array allows access to thruster values
thrusters = ['port', 'starboard', 'sway_fore', 'sway_aft']

heading_rate_var = rec_get_attr(sensors["heading_rate"])
pitch_rate_var = rec_get_attr(sensors["pitch_rate"])
roll_rate_var = rec_get_attr(sensors["roll_rate"])

depth_var = rec_get_attr(sensors["depth"])
depth_offset_var = rec_get_attr(sensors["depth_offset"])

quat_group = rec_get_attr(sensors["quaternion"])

# Use velocity from the DVL if the vehicle has one
vel_vars = {}
for vel_var in ["velx", "vely", "velz"]:
  if dvl_present:
    if vel_var in sensors:
      vel_vars[vel_var] = rec_get_attr(sensors[vel_var])
    else:
      raise LookupError("vehicle.dvl_present is True but vehicle.sensors.%s is "
                        "not defined" % vel_var)
  else:
    vel_vars[vel_var] = rec_get_attr("kalman." + vel_var)

# DVL Beam vars
beam_vars = [shm.dvl.low_amp_1,
        shm.dvl.low_amp_2,
        shm.dvl.low_amp_3,
        shm.dvl.low_amp_4,
        shm.dvl.low_correlation_1,
        shm.dvl.low_correlation_2,
        shm.dvl.low_correlation_3,
        shm.dvl.low_correlation_4]

control_wrench = shm.control_internal_wrench

quat_mode = shm.settings_control.quat_pid.get()
pass_through = False

def fx_quat(x, dt):
    q_initial, ang_vel = Quaternion(q=x[:4], unit=False), x[4:]

    # I don't think the below is correct.
    # Because HPR != axis of angular velocity
    # TODO XXX FIX
    # disp_quat = Quaternion(hpr=([math.degrees(vel * dt) for vel in ang_vel]))
    q_final = q_initial # * disp_quat
    x[0] = q_final[0]
    x[1] = q_final[1]
    x[2] = q_final[2]
    x[3] = q_final[3]
    return x

def hx_quat(x):
    return x

def fx_euler(x, dt):
    # x[0] += x[3]*dt
    # x[1] += x[4]*dt
    # x[2] += x[5]*dt

    # x[0] = x[0] % 360
    return x

def hx_euler(x):
    return x

def get_orientation(quat_values):
    q_offset = Quaternion(hpr=(shm.kalman_settings.heading_offset.get(), 0, 0))
    quat = Quaternion(q=[quat_values.q0, quat_values.q1, quat_values.q2, quat_values.q3])
    return q_offset * quat


quat_values = quat_group.get()
quat = get_orientation(quat_values).q
quat_orientation_filter = UnscentedKalmanFilter(7, fx_quat, 7, hx_quat, dt, .1)
quat_orientation_filter.x_hat = np.array([quat[0], quat[1], quat[2], quat[3], 0, 0, 0])
quat_orientation_filter.P *= .5
quat_orientation_filter.R = np.array([[90, 0, 0, 0, 0, 0, 0],
                                      [0, 90, 0, 0, 0, 0, 0],
                                      [0, 0, 90, 0, 0, 0, 0],
                                      [0, 0, 0, 90, 0, 0, 0],
                                      [0, 0, 0, 0, 1.5, 0, 0],
                                      [0, 0, 0, 0, 0, 1.7, 0],
                                      [0, 0, 0, 0, 0, 0, 1.5]])

quat_values = quat_group.get()
hpr = get_orientation(quat_values).hpr()
euler_orientation_filter = UnscentedKalmanFilter(6, fx_euler, 6, hx_euler, dt, .1)
euler_orientation_filter.x_hat = np.array([hpr[0], hpr[1], hpr[2], 0, 0, 0])
euler_orientation_filter.P *= .5
#TODO Fill in covariances-- ordering is H P R Hrate Prate Rrate
euler_orientation_filter.R = np.array([[100, 0, 0, 0, 0, 0],
                                       [0, 100, 0, 0, 0, 0],
                                       [0, 0, 100, 0, 0, 0],
                                       [0, 0, 0,  100, 0, 0],
                                       [0, 0, 0,  0, 100, 0],
                                       [0, 0, 0,  0, 0, 100]])


def convert_dvl_velocities(sub_quat, dvl_vel):
    # TODO This transform should maybe be in a configuration file.
    # Or perhaps we should configure the DVL to do it for us.
    vel_body_frame = Quaternion(hpr=(0, 0, 180)) * dvl_vel
    hpr = sub_quat.hpr()
    vel_spitz_frame = (Quaternion(hpr=(hpr[0]%360, 0, 0)).conjugate() * sub_quat) * vel_body_frame
    return vel_spitz_frame

def get_velocity(sub_quat):
    vel = np.array([-vel_vars[vel_var].get() for vel_var in \
                    ["velx", "vely", "velz"]])
    if dvl_present:
        # Rotate DVL velocities
        # vel[0] *= -1
        # vel[1] *= -1

        vel = convert_dvl_velocities(sub_quat, vel)

    return vel

def get_depth():
    return depth_var.get() - depth_offset_var.get()

sub_quat = Quaternion(q=quat_orientation_filter.x_hat[:4])

x_vel, y_vel, z_vel = get_velocity(sub_quat)

depth = get_depth()
kalman_xHat = np.array([[ x_vel, 0, y_vel, 0, 0, 0, depth, 0]]).reshape(8, 1)
# Pass in ftarray, shared memory handle to controller
position_filter = PositionFilter(kalman_xHat)

start = time.time()

show_rate = False
real_start = time.time()

last_start = 0
start = 0
iteration = 0
while True:
    # TODO Should we wait on gx4 group write?
    last_start = start
    time.sleep(max(0, dt-(start - last_start)))
    start = time.time()
    if True: # Pls forgive iteration*dt < time.time() - start:
        # Avoid timing errors due to time jumps on startup.
        # if time.time() - start - iteration*dt > 60:
        #     start = time.time()
        #     iteration = 0


        heading_rate_in = math.radians(heading_rate_var.get())
        pitch_rate_in = math.radians(pitch_rate_var.get())
        roll_rate_in = math.radians(roll_rate_var.get())
        

        # Bugs arise due to quaternion aliasing, so we choose the quaternion
        # closest to the actual state
        quat_values = quat_group.get()
        actual_quat = get_orientation(quat_values).q
        negated_quat = [-i for i in actual_quat]
        kalman_quat = None

        if quat_mode:
            kalman_quat = quat_orientation_filter.x_hat[:4]
        else:
            kalman_quat = Quaternion(hpr=euler_orientation_filter.x_hat[:3])
            kalman_quat = kalman_quat.q

        actual_delta = [kalman_quat[i] - actual_quat[i] for i in range(4)]
        negated_delta = [kalman_quat[i] - negated_quat[i] for i in range(4)]

        quat_in = actual_quat
        if np.linalg.norm(actual_delta) > np.linalg.norm(negated_delta):
            quat_in = negated_quat

        outputs = shm.kalman.get()

        if shm.settings_kalman.pass_through.get():
            print("Just passin through...")
            pass_through = True

            old_depth = outputs.depth
            old_east = outputs.east
            old_north = outputs.north

            sub_quat = Quaternion(q=quat_in)
            vels = get_velocity(sub_quat)
            hpr = sub_quat.hpr()
            c = math.cos(math.radians(hpr[0]))
            s = math.sin(math.radians(hpr[0]))
            north_vel = vels[0]*c - vels[1]*s
            east_vel = vels[0]*s + vels[1]*c

            outputs.accelx = 0
            outputs.accely = 0
            outputs.accelz = 0
            outputs.depth = get_depth()
            outputs.depth_rate = (outputs.depth - old_depth)/dt
            outputs.east = outputs.east + east_vel*dt
            outputs.forward = outputs.forward
            outputs.heading = hpr[0]
            outputs.heading_cumulative = 0
            outputs.heading_rate = heading_rate_in
            outputs.north = outputs.north + north_vel*dt
            outputs.pitch = hpr[1]
            outputs.pitch_rate = pitch_rate_in
            outputs.q0= quat_in[0]
            outputs.q1= quat_in[1]
            outputs.q2= quat_in[2]
            outputs.q3= quat_in[3]
            outputs.roll = hpr[2]
            outputs.roll_rate = roll_rate_in
            outputs.sway = outputs.sway 
            outputs.velx = vels[0]
            outputs.vely = vels[1]
            outputs.velz = vels[2]

            shm.kalman.set(outputs)

            continue
        else:
            if pass_through:
                pass_through = False

                sub_quat = Quaternion(q=quat_in)
                vels = get_velocity(sub_quat)
                hpr = sub_quat.hpr()
                c = math.cos(math.radians(hpr[0]))
                s = math.sin(math.radians(hpr[0]))
                north_vel = vels[0]*c - vels[1]*s
                east_vel = vels[0]*s + vels[1]*c

                # euler_orientation_filter.x_hat = np.array([hpr[0], hpr[1], hpr[2], heading_rate_in, pitch_rate_in, roll_rate_in])
                # quat_orientation_filter.x_hat = np.array([quat_in[0], quat_in[1], quat_in[2], quat_in[3], heading_rate_in, pitch_rate_in, roll_rate_in])
                # position_filter.xHat = np.array([[ north_vel, 0, east_vel, 0, 0, 0, get_depth(), outputs.depth_rate]]).reshape(8, 1)


        if shm.settings_control.quat_pid.get():
            if not quat_mode:
                # If we just switched, need to ensure that states between filters agree!
                q = Quaternion(hpr=euler_orientation_filter.x_hat[:3])
                q_state = list(q.q) + list(euler_orientation_filter.x_hat[3:])
                quat_orientation_filter.x_hat = q_state
                quat_mode = True

            quat_orientation_filter.predict()

            # TODO: It doesn't make sense to update regardless of whether there is new sensor data
            quat_orientation_filter.update(list(quat_in) + [heading_rate_in, pitch_rate_in, roll_rate_in])

            data = quat_orientation_filter.x_hat
            sub_quat = Quaternion(q=data[:4])

            # Stands for heading-pitch-roll
            hpr = sub_quat.hpr()

            keys = ['q0', 'q1', 'q2', 'q3', 'heading_rate', 'pitch_rate', 'roll_rate']
            output = dict(zip(keys, data))
            outputs.update(**output)
            outputs.heading_rate = math.degrees(outputs.heading_rate)
            outputs.pitch_rate = math.degrees(outputs.pitch_rate)
            outputs.roll_rate = math.degrees(outputs.roll_rate)
            outputs.update(**{'heading': hpr[0] % 360,
                              'pitch':   hpr[1],
                              'roll':    hpr[2]})

        else:
            if quat_mode:
                # If we just switched, need to ensure that states between filters agree!
                hpr = Quaternion(q=quat_orientation_filter.x_hat[:4])
                hpr_state = list(hpr.hpr()) + list(quat_orientation_filter.x_hat[4:])
                euler_orientation_filter.x_hat = hpr_state
                quat_mode = False

            euler_orientation_filter.predict()

            quat_in_old = quat_in

            quat_in = Quaternion(q=list(quat_in))
            hpr_in = quat_in.hpr()

            euler_orientation_filter.update([hpr_in[0] % 360, hpr_in[1], hpr_in[2], heading_rate_in, pitch_rate_in, roll_rate_in])

            data = euler_orientation_filter.x_hat

            hpr_quat = Quaternion(hpr=data[:3])
            quat = hpr_quat.q

            outputs = shm.kalman.get()
            keys = ['heading', 'pitch', 'roll', 'heading_rate', 'pitch_rate', 'roll_rate']

            deadband = 50
            in_bad_zone = hpr_in[0] % 360 < (0 + deadband) or hpr_in[0] % 360 > (360 - deadband)
            if in_bad_zone:
                data =  [hpr_in[0] % 360, hpr_in[1], hpr_in[2], heading_rate_in, pitch_rate_in, roll_rate_in]
                outputs.update(**{'q0': quat_in_old[0],
                                  'q1': quat_in_old[1],
                                  'q2': quat_in_old[2],
                                  'q3': quat_in_old[3]})
            else:
                outputs.heading_rate = math.degrees(outputs.heading_rate)
                outputs.pitch_rate = math.degrees(outputs.pitch_rate)
                outputs.roll_rate = math.degrees(outputs.roll_rate)
                outputs.update(**{'q0': quat[0],
                                  'q1': quat[1],
                                  'q2': quat[2],
                                  'q3': quat[3]})

            output = dict(zip(keys, data))
            outputs.update(**output)


        x_vel, y_vel, z_vel = get_velocity(sub_quat)

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


        # Check whether the DVL beams are good
        beams_good = sum( [not var.get() for var in beam_vars] ) >= 2

        # And if not, disable them
        if not beams_good and dvl_present:
            # This multiplies x and y velocity by 0 in the measurement vector,
            # but leaves x and y acceleration, and depth
            active_measurements = np.array([0,1,0,1,1]).reshape((5,1))
        else:
            active_measurements = None

        soft_kill = shm.switches.soft_kill.get()

        curr_thrusters = dict((t,(1-soft_kill)*shm.motor_desires.__getattribute__(t).get()) for t in thrusters)
        u = np.array((control_wrench.f_x.get(), control_wrench.f_y.get(), \
                      control_wrench.f_z.get(), control_wrench.t_x.get(), \
                      control_wrench.t_y.get(), control_wrench.t_z.get()))

        depth = get_depth()

        # Update
        # TODO: It doesn't make sense to update regardless of whether there is new sensor data
        outputs.update(**position_filter.update(outputs.heading, x_vel, x_acc,
                                                y_vel, y_acc, depth, u,
                                                active_measurements,
                                                curr_thrusters,
                                                outputs.pitch, outputs.roll))

        outputs.velz = z_vel
       
        # This really shouldn't be necessary when kalman has a u term (which it does)
        if not beams_good and dvl_present:
            outputs.velx = 0.0
            outputs.vely = 0.0

        # Write outputs as group, notify only once
        shm.kalman.set(outputs)

        iteration += 1

        if show_rate:
            if (iteration % 100 == 0):
                iteration = 0
                real_start = time.time()
            print(iteration/(real_start-time.time()))
