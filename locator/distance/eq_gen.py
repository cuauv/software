#!/usr/bin/env python2

from pylab import *
from scipy import *
from scipy import optimize
import argparse
import sys
import os

def isRectangular(obj):
    return (obj == 'torpedo' or obj == 'emperor' or  obj == 'wire')

def distancePrep(rawData):
    length = []
    distance = []
    for line in rawData:
        data = line.split(',')
        if float(data[0]) > 0:
            length.append(float(data[0]))
            distance.append(float(data[1]))
    return (length,distance)

def anglePrep(rawData):
    aspect = []
    angle = []
    for line in rawData:
        data = line.split(',')
        if float(data[0]) > 0:
            aspect.append(float(data[2]))
            angle.append(float(data[3]))
    return (aspect, angle)

# Generate data points with noise
# Read in csv file and set length and width

class EqGen:
    def __init__(self, obj):
        self.out_distance = None
        self.distance_archive = None
        self.out_angle = None
        self.angle_archive = None

        rawData = self.openProcessFiles(obj)
        
        lengthAndDistance = distancePrep(rawData)
        self.distanceEq(lengthAndDistance[0], lengthAndDistance[1])

        if (isRectangular(obj)):
            aspectAndAngle = anglePrep(rawData)
            self.angleEq(aspectAndAngle[0], aspectAndAngle[1])

        self.closeFiles(obj)

    def openProcessFiles(self, obj):
        if (obj == 'orange_buoy'):
            csv_file = open('orange_buoy.csv','r')
            self.out_distance = open('orange_buoy_dist','w')
            self.distance_archive = open('orange_buoy_dist_archive','a')
        elif (obj == 'green_buoy'):
            csv_file = open('green_buoy.csv','r')
            self.out_distance = open('green_buoy_dist','w')
            self.distance_archive = open('green_buoy_dist_archive','a')
        elif (obj == 'yellow_buoy'):
            csv_file = open('yellow_buoy.csv','r')
            self.out_distance = open('yellow_buoy_dist','w')
            self.distance_archive = open('yellow_buoy_dist_archive','a')
        elif (obj == 'torpedo'):
            csv_file = open('torpedo.csv','r')
            self.out_distance = open('torpedo_dist','w')
            self.distance_archive = open('torpedo_dist_archive','a')
            self.out_angle = open('torpedo_angle','w')
            self.angle_archive = open('torpedo_angle_archive','a')
        elif (obj == 'wire'):
            csv_file = open('wire.csv','r')
            self.out_distance = open('wire_dist','w')
            self.distance_archive = open('wire_dist_archive','a')
            self.out_angle = open('wire_angle','w')
            self.angle_archive = open('wire_angle_archive','w')
        elif (obj == 'emperor'):
            csv_file = open('emperor.csv','r')
            self.out_distance = open('emperor_dist','w')
            self.distance_archive = open('emperor_dist_archive','a')
            self.out_angle = open('emperor_angle','w')
            self.angle_archive = open('emperor_angle_archive','a')

        rawData = csv_file.readlines()[1:]
        csv_file.close()
        return rawData

    def distanceEq(self, length, distance):
        # Fit the first set
        fitfunc = lambda p, x: p[0]/x + p[1] # Target function
        errfunc = lambda p, x, y: fitfunc(p, x) - y # Dist to target function
        p0 = [50,1] # Initial guess for the parameters
        p1, success = optimize.leastsq(errfunc, p0[:], args=(length, distance))
        print p1

        toWrite = str(p1[0]) + ' ' + str(p1[1])
        self.out_distance.write(toWrite)
        self.distance_archive.write('\n')
        self.distance_archive.write(toWrite)
    
    def angleEq(self, aspect, angle):
        fitfunc = lambda p, x: p[0]/x + p[1] # Target function
        errfunc = lambda p, x, y: fitfunc(p, x) - y # Dist to target function
        p0 = [50,1] # Initial guess for the parameters
        p1, success = optimize.leastsq(errfunc, p0[:], args=(aspect, angle))
        print p1

        toWrite = str(p1[0]) + ' ' + str(p1[1])
        self.out_angle.write(toWrite)
        self.angle_archive.write('\n')
        self.angle_archive.write(toWrite)

    def closeFiles(self,obj):
        self.out_distance.close()
        self.distance_archive.close()
        if (isRectangular(obj)):
            self.out_angle.close()
            self.angle_archive.close()


if __name__ == "__main__":
    eq = EqGen(sys.argv[1])
    print "Equation generation complete."

