#!/usr/bin/env python2

import argparse 
parser = argparse.ArgumentParser(description="""Stress test for comparing OpenCL to NumPy""")
parser.add_argument('--size', type=int, default=1000, help="size of the grid", nargs='?')

args = parser.parse_args()

#### Configuration
SIZE = args.size # Size of the grid in cells
LENGTH = 10 # Length of the grid in meters
COUNT = 100 # Number of iterations to run in each test


#### NumPy-Based Version
import pylab
from math import radians, pi

w,h = SIZE, SIZE
scale = SIZE/float(LENGTH)
Xs, Ys = pylab.meshgrid( range(0,w), range(0,h) )
H = pylab.ones( Xs.shape )
def test(x_offset, y_offset, angle, min_distance = 0.25, max_distance = 3.6):
    max_dist = (max_distance*scale) ** 2
    min_dist = (min_distance*scale) ** 2

    xs = Xs - x_offset
    ys = Ys - y_offset

    angles = pylab.arctan2( xs, ys )
    angles =  (angles - radians(angle)) % (2*pi)
    angles -= 2*pi * (angles > pi)

    evidence = wide_angle_function(angles)

    dist = xs**2 + ys**2

    evidence *= pylab.logical_and( dist < max_dist, dist > min_dist)

    return evidence
def wide_angle_function(angles):
    return pylab.maximum(pylab.minimum(1.05-2*pylab.absolute(angles),1), 0)

import locator_cl
locator = locator_cl.Locator( 0,0, LENGTH, SIZE, 1)

import time

if __name__ == '__main__':
    print "Running with SIZE = %s" % SIZE
    start = time.time()
    for i in range(COUNT):
        locator.update( (0,0), 0, 0.5,2.5, 1.05, 0.8,0.2)
    end = time.time()
    print "OpenCL version took %s seconds to run %s iterations"%(end-start, COUNT)

    start = time.time()
    for i in range(COUNT):
        H *= 0.6*test(0,0, 0, 0.5,2.5) + 0.2;
    end = time.time()
    print "NumPy version took %s seconds to run %s iterations"%(end-start, COUNT)
