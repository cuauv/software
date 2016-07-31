from pylab import *
from scipy import *
from scipy import optimize
import argparse
import sys
import os

# Generate data points with noise
# Read in csv file and set length and width

"""
This program reads in a set of points from a csv file and interprets these points.
These points correspond to a torpedo, emperor, or wire.

Goal: Calculate aspect ratio and height.
Output: Aspect ratio and height (written to a csv file).

"""

# c**2 = a**2 + b **2 - 2ab cos (theta)
# theta = arc cos((c**2 - a**2 - b**2)/ -2ab)
# Returns the angle of a triangle with hypoteneus c and legs a and b
def lawOfCos(c,a,b):
    try:
        return acos((c**2 - a**2 - b**2)/(-2.*a*b))
    finally:
        # No angle
        return 0
    
# c = sqrt(a**2 + b**2)
# input: x and y of both points
# output: distance between points
def distance(a_x,a_y,b_x,b_y):
    return math.sqrt((b_y - a_y)**2. + (b_x - a_x)**2.)

parser = argparse.ArgumentParser()
parser.add_argument('-r','--redTorpedo',action = "store_true")
parser.add_argument('-b','--blueTorpedo',action= "store_true")
parser.add_argument('-w','--wire',action = "store_true")
parser.add_argument('-e','--emperor',action = "store_true")
args = parser.parse_args()

if (args.redTorpedo):
    csv_file = open('red_torpedo_raw_data.csv','r')
    out_file = open('red_torpedo.csv','w')
elif (args.blueTorpedo):
    csv_file = open('blue_torpedo_raw_data.csv','r')
    out_file = open('blue_torpedo.csv','w')
elif (args.wire):
    csv_file = open('wire_raw_data.csv','r')
    out_file = open('wire.csv','w')
elif (args.emperor):
    csv_file = open('emperor_raw_data.csv','r')
    out_file = open('emperor.csv','w')

raw_data = csv_file.readlines()
csv_file.close()

#process raw data
for line in raw_data:
    split_line = line.split(',')
    tl_x = split_line[0]
    tl_y = split_line[1]
    tr_x = split_line[2]
    tr_y = split_line[3]
    bl_x = split_line[4]
    bl_y = split_line[5]
    br_x = split_line[6]
    br_y = split_line[7]
    north_dist = split_line[8]
    east_dist = split_line[9]
    total_dist = split_line[10]

    #width and heights
    tw = distance(tl_x,tl_y,tr_x,tr_y)
    top_width = tw
    bw = distance(bl_x,br_y,bl_x,bl_y)
    bottom_width = bw
    lh = distance(tl_x,tl_y,bl_x,bl_y)
    left_height = lh
    rh = distance(tr_x,tr_y,br_x,br_x)
    right_height = rh

    #average height edge
    #used for distance to center of the object
    average_edge = (lh + rh) / 2

    #aspect ratio
    larger_width = max(tw,bw)
    larger_height = max(lh,rh)
    aspect = larger_width/larger_height

    #angle to object
    angle = math.atan2(north,east)

    csv_line = [average_edge,aspect,angle]
    out_file.write(csv_line)

out_file.close()
"""
#unused for now
#finds angles of each edge of perceived quadrilateral
for i in range(len(raw_data)):

    #diagonals
    tl_diag, br_diag = distance(tr_x[i],tr_y[i],bl_x[i],bl_y[i])
    tr_diag, bl_diag = distance(tl_x[i],tl_y[i],br_x[i],br_y[i])

    #angles
    tla = lawOfCos(tl_diag,tw,lh)
    tl_angle[i] = tla
    tra = lawOfcos(tr_diag,tw,rh)
    tr_angle[i] = tra
    bla = lawOfCos(bl_diag,bw,lh)
    bl_angle[i] = bla
    bra = lawOfCos(br_diag,bw,rh)
    br_angle[i] = bra
"""




