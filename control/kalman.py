import math
import time

#import numdifftools as ndt
import numpy as np
import shm

from auv_math.quat import Quaternion, quat_from_axis_angle

# Let's take into account the centrifugal relief that is produced by the
# Earth's rotation.
def gravity_mag(latitude):
  return 9.806 - 0.026 * math.cos(latitude * math.pi / 90)

# Ithaca, NY
#gravity = np.array((0.0, 0.0, gravity_mag(42.4433)))
gravity = np.array((0.0, 0.0, 9.61))
#mag = np.array((5.0, -5.592, 20.227)) # TODO measure this, is declination important?
mag = np.array((-1.155, -13.705, -39.111))
dt = 0.01

accel_vars = (shm.imu.accel_x, shm.imu.accel_y, shm.imu.accel_z)
gyro_vars = (shm.imu.gyro_x, shm.imu.gyro_y, shm.imu.gyro_z)
mag_vars = (shm.imu.mag_x, shm.imu.mag_y, shm.imu.mag_z)

generic_get = lambda arr: [v.get() for v in arr]
get_acc = lambda: generic_get(accel_vars)
get_gyro = lambda: generic_get(gyro_vars)
get_mag = lambda: generic_get(mag_vars)

def get_quat_ang_vel(state):
  return Quaternion(state[:4]), state[4:]

def make_state(q, ang_vel):
  return np.concatenate((q.q, ang_vel))

def predict(state):
  """ Predicts the next state given the current, f in the literature.
      State is a Numpy array and dt is float in seconds. """
  quat, angular_vel = get_quat_ang_vel(state)
  angle = np.linalg.norm(angular_vel)
  if abs(angle) < 1e-7:
    return state

  axis = angular_vel / angle;
  rotate_quat = quat_from_axis_angle(axis, angle * dt)
  return make_state(rotate_quat * quat, angular_vel)

def expected_measurements(state):
    """ Predicts the measurements given the current state, h in the literature
        State is a Numpy array. """
    quat, angular_vel = get_quat_ang_vel(state)
    accels = quat.conjugate() * gravity
    gyros = quat.conjugate() * angular_vel
    mags = quat.conjugate() * mag
    return np.concatenate((accels, gyros, mags))

def predict_from_ref(measured, reference):
  ref_norm = reference / np.linalg.norm(reference)
  measured_norm = measured / np.linalg.norm(measured)

  axis = np.cross(measured_norm, ref_norm)
  angle = np.arccos(ref_norm.dot(measured_norm))

  return quat_from_axis_angle(axis, angle)

def predict_from_accel(accel):
  return predict_from_ref(accel, gravity)

def predict_from_mag(mags):
  return predict_from_ref(mags, mag)

def accel_gyro_filter(state, accel, gyro, mags):
  acc_predict = predict_from_accel(accel)
  mag_predict = predict_from_mag(mags)
  state_predict = predict(state)
  gyro_predict = get_quat_ang_vel(state_predict)[0]
  return make_state(Quaternion(q=0.9*gyro_predict.q + 0.1*acc_predict.q), gyro)

if __name__ == "__main__":
  state = np.array((1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))

  P = np.eye(7)
  Q = np.eye(7)
  R = np.eye(9)
  I = np.eye(7)

  #F = ndt.Jacobian(predict)
  #H = ndt.Jacobian(expected_measurements)

  while 1:
    t = time.time()
    #ang_vel = np.array(get_gyro())
    #state = np.concatenate((state[:4], ang_vel))
    #print get_quat_ang_vel(state)[0].hpr()

    accs = get_acc()
    gyros = get_gyro()
    gyros[0] -= 0.073
    gyros[1] += 0.056
    gyros[2] += 0.151
    mags = get_mag()
    #z = np.concatenate((accs, gyros, mags))

    #F_curr = F(state)

    #state_predict = predict(state)
    #P_predict = F_curr.dot(P).dot(F_curr.T) + Q
    #H_curr = H(state_predict)

    #innovation = z - expected_measurements(state_predict)
    #S = H_curr.dot(P_predict).dot(H_curr.T) + R
    #K = P_predict.dot(H_curr.T).dot(np.linalg.inv(S))

    #state = state_predict + K.dot(innovation)
    #P = (I - K.dot(H_curr)).dot(P_predict)
    state = accel_gyro_filter(state, accs, gyros, mags)
    q = get_quat_ang_vel(state)[0]
    hpr = q.hpr()
    shm.navigation_desires.heading.set(hpr[0])
    shm.navigation_desires.pitch.set(hpr[1])
    shm.navigation_desires.roll.set(hpr[2])

    #shm.kalman.q0.set(state[0])
    #shm.kalman.q1.set(state[1])
    #shm.kalman.q2.set(state[2])
    #shm.kalman.q3.set(state[3])

    diff = time.time() - t
    if dt - diff > 0:
      time.sleep(dt - diff)
