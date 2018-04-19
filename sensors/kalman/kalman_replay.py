#!/usr/bin/env python2
'''
Reads a data log file and runs the kalman heading filter on it.

Pass in the data log filename as an argument.
Data log files can be recorded with util/data_logger.py
This must be a tuple, first element a dictionary of variable names to
arrays of data. Second element, an ordered list of 
'''
from math import degrees

#settings
heading_var = "hdg_dvl_linear"

#Get input filename
import sys
if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    print "Give data log file as a parameter."
    print "Data log files can be recorded with util/data_logger.py"
    exit()

#load File
import pickle
data = pickle.load(file(filename, "r"))

#Convert data
data["rate_3dmg"] = [degrees(x) for x in data["rate_3dmg"]]

#Make filter
import kalman_orientation
import numpy

#Construct xHat from first data record and zeros for pitch/roll
xHat = numpy.array([data[heading_var][0], (data["rate_3dmg"][0]), 0,0,0,0]).reshape(6,1)
kalman_orientation.initialize(xHat)

#Result storage
heading_list = []
rate_list = []


#Actually run the filter
for i in range(1,len(data[heading_var])):
    input = [data[heading_var][i], data["rate_3dmg"][i], data["rate_3dmg"][i],0,0,0,0]
    output = kalman_orientation.update(*tuple(input))
    heading_list.append( output[0][0] )
    rate_list.append( output[1][0] )

import pylab
#pylab.plot([(x-(data["hdg_dvl_linear"][0]-data["hdg_3dmg"][0]))%360.for x in data["hdg_dvl_linear"]], label="hm")
pylab.plot(data["hdg_dvl_linear"], label="hl")
pylab.plot(data["hdg_3dmg"], label="h3")
pylab.plot(heading_list, label="hk")
#pylab.show()
pylab.plot(data["rate_3dmg"], label="rm")
pylab.plot([degrees(x) for x in data["rate_imu"]], label="ri")
pylab.plot(rate_list, label="rk")
pylab.legend()
pylab.show()
