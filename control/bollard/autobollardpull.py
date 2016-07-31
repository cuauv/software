#!/usr/bin/env python2
import difflib
import numpy as np
import shm.motor_desires
import shm
import pylab
import pickle
import time
from time import sleep

# *******************************************************
#
# Program for conducting automated bollard-pull tests
# and auto generating 2nd order curve fits constants.
# 
# This program is meant to be used in conjunction with the
# bollard pull vision module to do a complete automatic bollard
# pull analysis of a thruster without human interaction
# 
# *******************************************************

def print_header(x):
    print "-" * len(x)
    print x
    print "-" * len(x)

print_header("Automated Bollard Pull Data Collection and Curve Fit Utility")
#excessively complex string comparison functional code to deteremine which thruster to control
ban_list = ["__", "watch", "ctype", "auv"]
thruster_input = raw_input("Which thruster do you want to test?\r\n> ")
thruster_list = filter(lambda z: not (reduce(lambda x,y: x or y, map(lambda x: x in z, ban_list))), dir(shm.motor_desires))
matches = map(lambda x: difflib.SequenceMatcher(a=x.lower(), b=thruster_input.lower()).ratio(), thruster_list)
motor_str = thruster_list[matches.index(max(matches))]
print ">> Interpreting your response as", motor_str.upper()
motor = shm._eval("motor_desires." + motor_str) #motor is the shared var we want to control

data = {} #data collection dictionary

RANGE_MIN = -255
RANGE_MAX =  255

exit_list = ["done", "finish", "exit", "bye"]

print_header("Begin data collection phase.")

RISE_DELAY = 10
SAMPLES = 5
SAMPLE_INTERVAL = 0.5
TIME_BETWEEN_SAMPLES = 5


def set_and_measure(x):

    print ">> Setting " + motor_str + " to " + str(x)

    motor.set(-x) ####

    sleep(RISE_DELAY)

    #Sample from bollard pull
    vals = []
    for i in range(0,5):
        vals.append(shm.bollard_results.value.get())
        sleep(SAMPLE_INTERVAL)

    y = float(sum(vals)) / len(vals)

    if x not in data.keys():
        data[x] = [y]
    else:
        data[x].append(y)
    print ">> Motor value " + str(x) + " mapped to force " + str(data[x])
    motor.set(0)


def save_data(name):

    #Do curve fits
    print "Data collected:"
    print " " + str(data)

    print "Averaged data:"
    def avg(x):
        return sum(x) / len(x)
    avg_values = map(avg, data.values())
    print " " + str(dict(zip(data.keys(), avg_values)))


    curve = np.polyfit(data.keys(), avg_values, 2)

    print "Polynomial fit to function a*x^2 + b*x + c:"
    print " a: " + str(curve[0])
    print " b: " + str(curve[1])
    print " c: " + str(curve[2])


    #save = ("y" in raw_input("Save data (y/n)?\r\n> ").lower())

    #save data!
    FILENAME = time.strftime("%Y-%m-%d_%H.%M.%S_" + name, time.localtime())

    with open(FILENAME + ".polynomial", "wb") as f:
        f.write(name + "\n")
        f.write("Polynomial fit to function a*x^2 + b*x + c:\n")
        f.write(" a: " + str(curve[0]) + "\n")
        f.write(" b: " + str(curve[1]) + "\n")
        f.write(" c: " + str(curve[2]) + "\n")

    pickle.dump([curve,data], open(FILENAME + ".pickle", "wb"))

    #graphical stuff
    x = pylab.arange(min(data.keys()), max(data.keys()), 0.01)
    y = curve[0] * x**2 + curve[1] * x + curve[2]

    pylab.plot(x,y)
    pylab.scatter(data.keys(), avg_values, c='r', marker='o')
    pylab.xlabel("PWM")
    pylab.ylabel("Scale Force (lbs)")
    pylab.title("Bollard Pull test: " + name)

    pylab.savefig(FILENAME + ".png")

    #pylab.show()

raw_input("Ready to begin. Push enter to start the automated testing.")
name = raw_input("Input name tag for test\n>")

#samples = range(-20,-255,-10)
#samples.append(-255)

samples = range(20,255,10)
samples.append(255)

#samples = [50, 100, 255]

for i in samples:
    set_and_measure(i)
    sleep(TIME_BETWEEN_SAMPLES)


print "DONE WITH SAMPLES"
save_data(name)






















