from pylab import *
from scipy import *
from scipy import optimize
import csv
import sys
import os

# Generate data points with noise
# Read in csv file and set length and width

"""
This program reads in a set of points from a csv file and interprets these points.
These points correspond to a torpedo, emperor, or wire.
Data is thrown out if the aspect ratio is less than 1

Goal: Calculate aspect ratio and height.
Output: Average height of left and right edge of rectange, aspect ratio and angle (written to a csv file).

Torpedo and emperor format: [max_height, actual_distance, aspect_ratio, angle_to_object]
Wire format: [bigger_height, actual_distance, aspect_ratio, angle_to_object]

"""

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
"""

# c = sqrt(a**2 + b**2)
# input: x and y of both points
# output: distance between points
def distance(a_x,a_y,b_x,b_y):
    return sqrt((b_y - a_y)**2. + (b_x - a_x)**2.)

class RectDataAnalysis:

    def __init__(self, obj):
        self.obj = obj
        if (obj == 'torpedo'):
            self.csv_file = open('torpedo_raw.csv','r')
            out_file = open('torpedo.csv','w')
        elif (obj == 'wire'):
            self.csv_file = open('wire_raw.csv','r')
            out_file = open('wire.csv','w')
        elif (obj == 'emperor'):
            self.csv_file = open('emperor_raw.csv','r')
            out_file = open('emperor.csv','w')

        self.csvwriter = csv.writer(out_file)
        self.processData()
        out_file.close()

    def processData(self):
        raw_data = self.csv_file.readlines()
        self.csv_file.close()
        
        #process raw data
        for line in raw_data:
            if (self.isSquare()):
                self.processSquare(line)
            if (self.obj == 'wire'):
                self.processWire(line)

    def isSquare(self):
        return (self.obj == 'torpedo' or self.obj == 'emperor')

    def processSquare(self, line):
        split_line = line.split(',')
        for i in range(len(split_line)):
            split_line[i] = float(split_line[i])
        tl_x = split_line[0]
        tl_y = split_line[1]
        tr_x = split_line[2]
        tr_y = split_line[3]
        bl_x = split_line[4]
        bl_y = split_line[5]
        br_x = split_line[6]
        br_y = split_line[7]
        north = split_line[8]
        east = split_line[9]
        total_dist = split_line[10]

        #width and heights
        tw = distance(tl_x,tl_y,tr_x,tr_y)
        bw = distance(bl_x,br_y,bl_x,bl_y)
        lh = distance(tl_x,tl_y,bl_x,bl_y)
        rh = distance(tr_x,tr_y,br_x,br_x)

        #aspect ratio
        bigger_width = max(tw,bw)
        bigger_height = max(lh,rh)
        aspect = bigger_height/bigger_width

        #angle to object
        angle = math.degrees(math.atan(abs(north/east)))

        if (aspect >= 1 and bigger_width > 0):
            csv_line = [bigger_height, total_dist, aspect, angle]
            self.csvwriter.writerow(csv_line)

    def processWire(self, line):
        split_line = line.split(',')
        for i in range (len(split_line)):
            split_line[i] = float(split_line[i])
        bigger_height = split_line[0]
        bigger_width = split_line[1]
        north = split_line[2]
        east = split_line[3]
        total_dist = split_line[4]
        
        #aspect ratio
        aspect = bigger_height/bigger_width

        #angle to object
        angle = math.degrees(math.atan(abs(north/east)))

        if (bigger_height > 0 and aspect > .5):
            csv_line = [bigger_height, total_dist, aspect, angle]
            self.csvwriter.writerow(csv_line)

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
