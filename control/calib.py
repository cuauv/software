import numpy as np
import random
import time
from auv_math import quat
import thruster_manager
import argparse # don't import auv_var_lib
from shm import gx4, kalman, motor_desires, desires, control_internal_wrench
import shm
import sys
import json 
# next steps:
# measure and print out least squares error for inverse
# for proof of correctness, can get angular accelerations from fishbowl
# can also get z accel from fishbowl
# make sure that with pulsing multiple thrusters, we get the correct output


""" Implementation of the calibration strategy in the following paper: http://www.doniec.de/download/DoniecDetweilerRus_ISER2010.pdf
"""
dt = 0.01
# Poll accelerations for 3 seconds
polltime = 0.5


measured_linear_debug_array = []
measured_rotational_debug_array = []
mode = "onebyone"

# Returns the orientation of the sub as a quaternion
def get_orientation():
    return quat.Quaternion(q=[
        kalman.q0.get(), 
        kalman.q1.get(), 
        kalman.q2.get(), 
        kalman.q3.get()
        ])

# Compute measured rotational acceleration (3x1 row vec)
def mes_rot_acc():
    v1 = get_ang_vel()
    time.sleep(dt)
    v2 = get_ang_vel()
    return (v2 - v1) / float(dt)

# Returns current angular velocity of the sub (3x1 row vec)
# These should be reported in the sub frame in the sim
def get_ang_vel():
    return np.array([
        kalman.roll_rate.get(),
        kalman.pitch_rate.get(), 
        kalman.heading_rate.get()
        ])

def mes_lin_acc(src):
    if src != "sim":
        return np.array([gx4.accelx.get(), gx4.accely.get(), gx4.accelz.get()])

    accelx = kalman.accelx.get()
    accely = kalman.accely.get()
    accelz = kalman.accelz.get()

    #z1 = kalman.depth.get()
    #time.sleep(dt)
    #z2 = kalman.depth.get()
    #time.sleep(dt)
    #z3 = kalman.depth.get()

    #dz1 = (z2 - z1) / dt
    #dz2 = (z3 - z2) / dt

    #accelz = (dz2 - dz1) / dt
    #accelz = 0
    return np.array([accelx, accely, accelz])

def measure_spikes(src):
    t1 = time.time()
    rot = []
    lin = []
    while time.time() - t1 < polltime:
        rot.append(mes_rot_acc())
        lin.append(mes_lin_acc(src))

    # temp debug serialization
    lincopy = [list(i) for i in lin]
    rotcopy = [list(i) for i in rot]
    measured_linear_debug_array.append(lincopy)
    measured_rotational_debug_array.append(rotcopy)

    # Should account for buoyancy forces
    # note: buoyancy only affects Z!!!!

    #hi = max(lin, key=np.linalg.norm)
    #lo = min(lin, key=np.linalg.norm)
    #max_lin = hi - lo
    loz = min(lin, key=lambda a: a[2])
    hi = max(lin, key=lambda a: max(abs(a[0]), abs(a[1]), abs(a[2])))
    max_lin = hi

    #hi = max(rot, key=np.linalg.norm)
    #lo = min(rot, key=np.linalg.norm)
    #max_rot = hi - lo
    hi = max(rot, key=lambda a: max(abs(a[0]), abs(a[1]), abs(a[2])))
    max_rot = hi

    return (max_lin, max_rot)

# Applies thrusts to the sub
def apply_thrusts(t, tm, src):

    if src == "sim":
        # Uncomment to test vectored thrusts
        #t = np.array(np.load("tz_wrench"))[0]
        #t /= np.linalg.norm(t)

        # from percentages to max thrusts
        t = 16 * t
        g = kalman.get()
        tm.update(g)
        outputs = tm.thrusts_to_sub.dot(t)

        d = {}
        d["fore_port"] = t[0]
        d["fore_starboard"] = t[1]
        d["aft_port"] = t[2]
        d["aft_starboard"] = t[3]
        d["port"] = t[4]
        d["starboard"] = t[5]
        d["sway_fore"] = t[6]
        d["sway_aft"] = t[7]

        print "applying thrusts:"
        for key in d:
            if abs(d[key]) > 0:
                print "%s%s: %f" % (key, (14 - len(key))*" ", d[key])
        print "applying wrench", outputs
        control_internal_wrench.f_x.set(outputs[0])
        control_internal_wrench.f_y.set(outputs[1])
        control_internal_wrench.f_z.set(outputs[2])
        control_internal_wrench.t_x.set(outputs[3])
        control_internal_wrench.t_y.set(outputs[4])
        control_internal_wrench.t_z.set(outputs[5])

    elif src == "sub" or src == "shmlog":
        m = motor_desires.get()
        t = [int(255 * i) for i in t]
        #print "Applying %s" % t
        #m.fore_port = t[0]
        #m.fore_starboard = t[1]
        #m.aft_port = t[2]
        #m.aft_starboard = t[3]
        #m.port = t[4]
        #m.starboard = t[5]
        #m.sway_fore = t[6]
        #m.sway_aft = t[7]

        # TODO adjust to broken thrusters from vehicle conf
        m.fore_port = t[0]
        #m.fore_starboard = t[1]
        #m.aft_port = t[2]
        m.aft_starboard = t[1]
        m.port = t[2]
        m.starboard = t[3]
        m.sway_fore = t[4]
        m.sway_aft = t[5]
        motor_desires.set(m)



def get_thrusts():
    t = np.zeros(8)
    m = motor_desires.get()
    # ordering here is from conf/thor.conf
    t[0] = m.fore_port
    t[1] = m.fore_starboard
    t[2] = m.aft_port
    t[3] = m.aft_starboard
    t[4] = m.port
    t[5] = m.starboard
    t[6] = m.sway_fore
    t[7] = m.sway_aft
    
    return t

# Compute (mean, stdev) of noise when resting, per dimension
# Returns [(x_accel_mean, x_accel_stdev), (y_accel_mean, ....]
def mle_gaussian(data):
    return [(np.mean(row), np.std(row)) for row in np.transpose(data)]

def linear_accel_debug_thrusts(N):
    thrusts = np.zeros(N)
    r = random.random()
    if r < 0.330:
        # hardcoded forward x thrusts
        thrusts[4] = 1
        thrusts[5] = 1
    elif r < 0.667:
        # hardcoded forward y thrusts
        thrusts[6] = 1
        thrusts[7] = 1
    else:
        # hardcoded forward z thrusts
        thrusts[0] = 1
        thrusts[1] = 1
        thrusts[2] = 1
        thrusts[3] = 1
    return thrusts

def angular_accel_debug_thrusts(N):
    thrusts = np.zeros(N)
    r = random.random()
    if r < 0.330:
        # hardcoded x torque thrusts
        thrusts[2] = 1 
        thrusts[3] = 1 
        thrusts[0] = -1
        thrusts[1] = -1
    elif r < 0.667:
        # hardcoded y torque thrusts
        thrusts[1] = 1
        thrusts[3] = 1
        thrusts[0] = -1
        thrusts[4] = -1
    else:
        # hardcoded z torque thrusts
        thrusts[6] = 0.5
        thrusts[7] = -0.5
    return thrusts

def pick(n):
    I = []
    if mode == "comb":
        for _ in range(n):
            # might want to use negatives
            #k = random.random()
            #if k < (1.0/3.0):
            #    I.append(1)
            #elif k < (2.0/3.0):
            #    I.append(0)
            #else:
            #    I.append(-1)
            I.append(1 if random.random() < 0.5 else 0)
        return np.array(I)
    else:
        I = np.zeros(n)
        I[int(random.random() * n)] = 1
        return I

# Returns whether the values of every element of reading are
# greater than k stdevs from their respective means in stats.
# CONDITION: 
# stats   = [(mean_i, stdev_i)...]
# reading = [reading_i...]
def sufficiently_different(reading, stats, k, src):
    if src == "sim" or src == "shmlog":
        return True
    for i, dimension in enumerate(reading):
        if abs(dimension) < stats[i][0] + k * stats[i][1]:
            return False
    return True

def stabilize(src):
    print "stabilizing..."
    shm.settings_control.enabled.set(True)
    desires.pitch.set(0)
    desires.roll.set(0)
    desires.speed.set(0)
    desires.sway_speed.set(0)
    desires.heading.set(desires.heading.get())
    desires.depth.set(2.5)
    STABILIZE_TIME = 10
    time.sleep(STABILIZE_TIME)
    shm.settings_control.enabled.set(False)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--src",  help="Calibration source. sub, sim, or shmlog")
    args = parser.parse_args()

    if not any([args.src == i for i in ["sim", "sub", "shmlog"]]):
        print "Invalid args! Try python calib.py --help"
        sys.exit()

    tm = thruster_manager.ThrusterManager()

    # calib source (simulator, shmlog, real sub)

    # N thrusters
    N = 6
    # 3 stdevs from mean indicates we have enough power
    k = 3

    # First measure the noise.
    noise_time  = 30
    rest_rot = []
    rest_lin = []
    stabilize(args.src)
    # we only bother with the noise parameters on the sub
    learning = args.src == "sub"
    start_time = time.time()
    while learning: 
        print "Measuring noise..."
        lin_acc, rot_acc = measure_spikes(args.src)
        rest_lin.append(lin_acc)
        rest_rot.append(rot_acc)

        if time.time() - start_time > noise_time:
            learning = False

    lin_noise_params = mle_gaussian(rest_lin)
    rot_noise_params = mle_gaussian(rest_rot)

    # % of max thrust; actual thruster ranges are -255 to 255
    power = 0.01
    picked = set()

    start_time = time.time()
    learning = True

    if mode == "comb":
        max_iter = 100
    else:
        max_iter = 8
    # Measured linear accelerations
    S_acc = np.empty((max_iter, 3))
    # Applied thruster values (meeting DVL noise standards)
    T_acc = np.empty((max_iter, N))

    # Measured angular accelerations
    S_rot = np.empty((max_iter, 3))
    # Applied thruster values (meeting gyro noise standards)
    T_rot = np.empty((max_iter, N))

    iterations = 0
    while iterations < max_iter:
        print "iteration: %d" % iterations
        I = None
        while I is None:
            candidate = pick(N)
            if tuple(candidate) not in picked:
                I = candidate
                picked.add(tuple(candidate))

        thrusts = I * power
        apply_thrusts(thrusts, tm, args.src)
        lin_acc, rot_acc = measure_spikes(args.src)
        print "measured lin acc total",  lin_acc
        print "measured rot acc total",  rot_acc
        stabilize(args.src)
        flag_acc = True
        flag_rot = True

        if sufficiently_different(lin_acc, lin_noise_params, k, args.src):
            #print "measured lin acc total",  lin_acc
            S_acc[iterations] = lin_acc
            if args.src == "sub" or args.src == "sim":
                T_acc[iterations] = thrusts
            elif args.src == "shmlog":
                T_acc[iterations] = get_thrusts()
            flag_acc = False

        if sufficiently_different(rot_acc, rot_noise_params, k, args.src):
            #print "measured rot acc total",  rot_acc
            S_rot[iterations] = rot_acc
            if args.src == "sub" or args.src == "sim":
                T_rot[iterations] = thrusts
            elif args.src == "shmlog":
                T_rot[iterations] = get_thrusts()

            flag_rot = False

        if flag_acc or flag_rot:
            power = min(power + 0.1, 1.0)
        else:
            iterations += 1


    if len(S_rot) > 0 and len(S_acc) > 0:

        with open('S_rot', 'wb') as f:
            np.save(f, S_rot)

        with open('S_acc', 'wb') as f:
            np.save(f, S_acc)

        with open('T_rot', 'wb') as f:
            np.save(f, T_rot)

        with open('T_acc', 'wb') as f:
            np.save(f, T_acc)

        with open("measured_linear_debug", "wb") as measured_linear_debug:
            json.dump(measured_linear_debug_array, measured_linear_debug)
            
        with open("measured_rotational_debug", "wb") as measured_rotational_debug:
            json.dump(measured_rotational_debug_array, measured_rotational_debug)

        A_rot = np.linalg.pinv(S_rot).dot(T_rot)
        A_trans = np.linalg.pinv(S_acc).dot(T_acc)
        print "A_rot ="
        print A_rot
        print "A_trans ="
        print A_trans

        with open('A_rot', 'wb') as f:
            np.save(f, A_rot)

        with open('A_trans', 'wb') as f:
            np.save(f, A_trans)

        A_control = np.concatenate((A_trans, A_rot), axis=0)
        with open('A_control', 'wb') as f:
            np.save(f, A_control)

    else:
        s  = "Unable to compute inverses; no data gathered different from noise. "
        s += "Gather more data."
        print s

