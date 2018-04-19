import numpy
import shm
import time
from mission.framework.taskclasses import *

yaw = [shm.kalman.heading, shm.navigation_desires.heading, shm.settings_heading]
pitch = [shm.kalman.pitch, shm.navigation_desires.pitch, shm.settings_pitch]
roll = [shm.kalman.roll, shm.navigation_desires.roll, shm.settings_roll]

depth = [shm.kalman.depth, shm.navigation_desires.depth, shm.settings_depth]
velx = [shm.kalman.velx, shm.navigation_desires.speed, shm.settings_velx]
vely = [shm.kalman.vely, shm.navigation_desires.sway_speed, shm.settings_vely]

# Profiles to tune each degree
# list: start value, start P, epsilon, setpoint, granularity
yaw_data = [180, .05, 4, 15, .015]
pitch_data = [0, .3, 4, 15, .04]
roll_data = [0, .3, 5, 15, .04]

velx_data = [0, 3, .04, .1, .5]
vely_data = [0, 10, .04, .1, 1]
depth_data = [1, 3, .1, .25, .5]


dt = .07
# epsilon = 5
# setpoint = 15
# granularity = .05
ACQUIRING_DATA = 0
CHANGING_P = 1
FINISHED = 2
UNSTABLE = 3


def analyze(data, epsilon, kP):
    zeroes = []
    zero_index = []
    count = -1
    prev = data[0]
    for i in range(len(data)): 
        count += 1
        if numpy.sign(prev) != numpy.sign(data[i]):
            zeroes.append(count)
            zero_index.append(i)
            count = 0
        prev = data[i]
    #The first zero is not necessarily reliable
    if len(zeroes) not in [0, 1]:
        avg = sum(zeroes[1:])/len(zeroes[1:])
        period = 2*avg*dt
        shm.trax.q2.set(period)
        abs_data = list(map((lambda x: abs(x)), data))
        sample = [max(abs_data[zero_index[i]:zero_index[i+1]]) for i in range(len(zero_index)-1)]
        sample_avg = sum(sample)/len(sample)
        diff =  abs(sample[0]) - abs(sample[-1])
        no_decay = abs(diff) < epsilon
        shm.trax.q0.set(diff)

        deviation = [abs(i - avg) for i in zeroes[1:]] 
        irreg = any(map(lambda x: x > 4, deviation))
        shm.trax.q1.set(max(deviation))
        print(deviation)


        if no_decay: #and not irreg:
            #Ziegler-Nichols Tuning values for various systems
            #Includes, P, PI, PD, PID, Pessen Integral, Some Over, No Over
            #return FINISHED, [.5*kP,0,0]
            #return FINISHED, [.45*kP,period/1.2,0]
            #return FINISHED, [.8*kP,0,period/8]
            return FINISHED, [.6*kP,period/2,period/8]
            #return FINISHED, [.7*kP,period/2.5,3*period/20]
            #return FINISHED, [.33*kP,period/2,period/3]
            #return FINISHED, [.2*kP,period/2,period/3]
        elif diff < -10:
            return UNSTABLE, None

    return CHANGING_P, None



class Tune(FiniteTask):
    def firstrun(self, variable, data):
        [self.reading, self.var, self.PID] = variable
        self.time = time.time()
        self.start = time.time()

        self.currentP = data[1]
        self.PID.kP.set(self.currentP)
        self.PID.kI.set(0)
        self.PID.kD.set(0)
        self.starting = [self.PID.kP.get(),self.PID.kI.get(),self.PID.kD.get()]
        self.sp = data[0]+data[3]
        self.var.set(self.sp)
        self.amplitude = data[3]

        self.state = CHANGING_P
        self.data = []
        self.results = [0, 0, 0]


    def run(self, variable, data):

        if self.state == ACQUIRING_DATA:
            self.log("Currently logging data at kP = {}".format(self.currentP))
            if self.this_run - self.time > dt:
                self.data.append(self.reading.get()-self.sp)
                self.time = time.time()
            if len(self.data) > 100:
                self.log("Analyzing collected data")
                self.state, self.results = analyze(self.data, data[2], self.currentP)
                if self.state == CHANGING_P:
                    self.currentP += data[4]
                    self.PID.kP.set(self.currentP)
                    self.amplitude *= -1
                    self.sp += 2*self.amplitude
                    self.var.set(self.sp)


        elif self.state == CHANGING_P:
            #Wait 2 seconds for IC to die away
            self.log("Changed kP to {}, waiting for SS Oscillations".format(self.currentP))
            if self.this_run - self.time > 3:
                self.time = time.time()
                self.state = ACQUIRING_DATA
                self.data = []


        elif self.state == FINISHED:
            self.log("Period is: {}. Found optimal PID values: {}".format(2*self.results[1], self.results))
            self.PID.kP.set(self.results[0])
            self.PID.kI.set(self.results[1])
            self.PID.kD.set(self.results[2])
            self.var.set(0)
            self._finish() 
        
        elif self.state == UNSTABLE:
            self.log("Unstable oscillations, aborting!")
            self.PID.kP.set(self.starting[0])
            self.PID.kI.set(self.starting[1])
            self.PID.kD.set(self.starting[2])
            self.var.set(0)
            self._finish() 



tune_roll = Tune(roll, roll_data)
tune_yaw = Tune(yaw, yaw_data)
tune_pitch = Tune(pitch, pitch_data)

tune_velx = Tune(velx, velx_data)
tune_vely = Tune(vely, vely_data)
tune_depth = Tune(depth, depth_data)

