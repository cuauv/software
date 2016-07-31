#!/usr/bin/env python2
import difflib
import numpy as np
import shm.motor_desires
import pylab
import pickle
import time

# *******************************************************
#
# Program for conducting automated bollard-pull tests
# and auto generating 2nd order curve fits constants.
# 
# *******************************************************

LEVER_ARM_SCALE = 9 / 18 

NEWTONS_PER_POUND = 4.44822
OUNCES_PER_POUND = 16.0

def print_header(x):
    print "-" * len(x)
    print x
    print "-" * len(x)

print_header("Automated Bollard Pull Data Collection and Curve Fit Utility")

pounds = raw_input("Will force be entered as pounds and ounces (y/n)?")
pounds = pounds in ["y", "Y", "yes", "Yes", "YES"]
if pounds:
    print("Interpreting force input as pounds followed by ounces")
else:
    print("Interpreting force input as Newtons.")

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

#Data collection loop
while True:
    try:
        cmd = raw_input("Specify a motor value, or \"help\" for command help:\r\n> ")
    except KeyboardInterrupt, EOFError:
        print ">> Exiting..."
        break

    #Handle Commands
    if reduce(lambda x,y: x or y, map(lambda x: x in cmd.lower(), exit_list)):
        print ">> Exiting..."
        break

    if "help" in cmd.lower():
        print ">> Enter a motor value between " + str(RANGE_MIN) + " and " + str(RANGE_MAX)
        print "   or one of the following commands:"
        print "    - \"done\" to stop data collection and execute the curve fit"
        print "    - \"off\" to turn the motor off without recording any data"
        print "    - \"clear [val]\" to clear all recorded values for PWM [val]"
        print "    - \"view [val]\" to view all recorded values for PWM [val]"
        print "    - \"import [file]\" to import a pickle file containing data"
        continue

    if "off" in cmd.lower():
        motor.set(0)
        print ">> Motor has been turned off"
        continue

    if "view" in cmd.lower():
        try:
            num = int(cmd.split(" ")[1])
        except:
            print ">> Invalid number format."
            continue
        if num not in data.keys():
            print " >> PWM " + str(num) + " is not mapped."
        else:
            print " >> PWM " + str(num) + " is mapped to " + str(data[num])
        continue

    if "clear" in cmd.lower():
        try:
            num = int(cmd.split(" ")[1])
        except:
            print ">> Invalid number format."
            continue
        print ">> Values for PWM " + str(num) + " have been cleared."
        del data[num]
        continue #TODO: Implement
    if "pickle" in cmd.lower():
        try:
            pickle_object = pickle.load(open(cmd.split(" ")[1], "rb"))
        except:
            print ">> Invalid file specified."
            continue

        data = pickle_object[1]
        print ">> Data file imported."
        continue
    
    #Collect data
    try:
        x = int(cmd)
    except ValueError:
        print ">> Invalid command. Input a number or \"done\" to exit."
        continue

    if x < RANGE_MIN or x > RANGE_MAX:
        print ">> Integer out of motor range!"
        continue

    print ">> Setting " + motor_str + " to " + str(x)

    motor.set(x)

    units = "Newtons" if not pounds else "pounds and ounces"
    val = raw_input("What force does the gauge read (in %s)?\r\n> " % units)
    try:
        if pounds:
            lbs, oz = map(float, val.strip().split())
            y = NEWTONS_PER_POUND * (lbs + oz/OUNCES_PER_POUND)
        else:
            y = float(val)
    except ValueError:
        print ">> Invalid response value"
        continue

    if x not in data.keys():
        data[x] = [y]
    else:
        data[x].append(y)
    print ">> Motor value " + str(x) + " mapped to force " + str(data[x])

print_header("Data collection complete!")


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


save = ("y" in raw_input("Save data (y/n)?\r\n> ").lower())


#save data!
FILENAME = time.strftime("%Y-%m-%d_%H.%M.%S_" + str(motor_str), time.localtime())

if save:
    name = raw_input("Input name tag for data\n>")
    FILENAME = time.strftime("%Y-%m-%d_%H.%M.%S_" + name, time.localtime())
    pickle.dump([curve,data], open(FILENAME + ".pickle", "wb"))

    with open(FILENAME + ".polynomial", "wb") as f:
        f.write(name + "\n")
        f.write("Polynomial fit to function a*x^2 + b*x + c:\n")
        f.write(" a: " + str(curve[0]) + "\n")
        f.write(" b: " + str(curve[1]) + "\n")
        f.write(" c: " + str(curve[2]) + "\n")

#graphical stuff
x = pylab.arange(min(data.keys()), max(data.keys()), 0.01)
y = curve[0] * x**2 + curve[1] * x + curve[2]

pylab.plot(x,y)
pylab.scatter(data.keys(), avg_values, c='r', marker='o')
pylab.xlabel("PWM")
pylab.ylabel("Force (Newtons)")
pylab.title("Bollard Pull test for " + motor_str.upper())

if save:
   pylab.savefig(FILENAME + ".png")

pylab.show()




















