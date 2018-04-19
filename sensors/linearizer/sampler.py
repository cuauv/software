#!/usr/bin/env python2

import shm
import math
from time import sleep
import pickle
import sys


if len(sys.argv) != 2:
    print "Usage: sampler.py [output pickle file]"
    sys.exit(0)

OUTPUT_FILE = sys.argv[1]

SAMPLES = 20 #Samples to obtain for each shared variable
SAMPLE_INTERVAL = 0.05 #seconds between samples

#Add heading vars here to get sampled output
heading_vars = {"3dmg": shm.threedmg.heading,
                "kalman": shm.kalman.heading,
                "sparton": shm.sparton.heading,
                "linearized": shm.linear_heading.heading,
                }

#############################################################

try:
    exists = open(OUTPUT_FILE, "r")
    data = pickle.load(exists)
    output_dictionary = {}
    for sensor in heading_vars.keys():
        output_dictionary[sensor] =  dict(zip(data[0],data[1][sensor]))
    print "Values loaded from file"

except IOError:
    output_dictionary = dict((x,{}) for x in heading_vars.keys())

var_to_lbl = dict(zip(heading_vars.values(), heading_vars.keys()))

#Write our output dictionary in a format that tom likes
def write_output_to_pickle():
    out_keys = output_dictionary.values()[0].keys() #a bit hacky
    out_keys.sort()
    val_dict = {} 
    for k in output_dictionary.keys():
        val_dict[k] = [output_dictionary[k][v] for v in out_keys]
    output = [out_keys, val_dict, 0.0]
    pickle.dump(output, open(OUTPUT_FILE, 'w+'))

def grab_average_headings(svars):
    x = dict((z,0) for z in svars)
    y = dict((z,0) for z in svars)
    for i in range(SAMPLES):
        for v in svars:
            heading = math.radians(v.get())
            y[v] += math.sin(heading)
            x[v] += math.cos(heading)
        sleep(SAMPLE_INTERVAL)
    return dict((v, (math.degrees(math.atan2(y[v],x[v])) + 360.0) % 360.0) for v in svars)

try:
    print "=== Heading Data Acquisition Tool ==="
    print "Results will be saved to %s" % OUTPUT_FILE
    print ""
    while True:
        print "Enter degrees for sample:"
        hdg_in = float(input(" > "))

        print "\tSampling for angle %.1f (keep sub still!)" % hdg_in
        vals = grab_average_headings(heading_vars.values())
        for (k,v) in vals.items():
            sname = var_to_lbl[k]
            print "\tAcquired sample: %s is %.1f" % (sname, v)
            output_dictionary[sname][hdg_in] = v
            #output_dictionary[sname][hdg_in] = hdg_in

        write_output_to_pickle()
        print "\tOutput file written"

except KeyboardInterrupt:
    print "\n\nInterrupt; exiting"
