#!/usr/bin/env python2
import shm
import argparse
import csv
import math
import sys 
import select
import tty
import termios
import rect_data_analysis
import eq_gen

"""
README

Script requires a tag associated with an object when executed in command line, and collects data associated with the object specified by the tag.
Must be used while data can be collected from shared memory.

Tag options:
    -o Orange Buoy
    -g Green Buoy
    -y Yellow Buoy
    -r Red Torpedo
    -b Blue Torpedo
    -w Wire
    -e Emperor

Each of these tags will write data (Distance and either points or lengths etc) to an associated CSV file.
Data is used to analyze the correlation between distance and some readily accessible data that vision can access in shm.
The data will be used to create equations that generalize these properties.

Outputs
buoy: [radius, total_distance]
emperor and torpedo: [tl_x, tl_y, tr_x, tr_y, bl_x, bl_y, br_x, br_y, north_dist, east_dist, total_distance]
wire: [bigger_height, bigger_width, north_dist, east_dist, total_distance]

Press ESC to quit (function currently not working)
"""

def isBuoy():
    return (args.orangeBuoy or args.greenBuoy or args.yellowBuoy)

def isSquare():
    return (args.redTorpedo or args.blueTorpedo or args.emperor)

def isRectangular():
    return (isSquare() or args.wire)

def isData():
    return select.select([sys.stdin], [], [], 0) == ([sys.stdin], [], [])

BEGIN_DIST_AWAY = .5
END_DIST_AWAY = 4.

parser = argparse.ArgumentParser(description="Target of interest")
parser.add_argument('-o','--orangeBuoy',action = "store_true")
parser.add_argument('-g','--greenBuoy',action = "store_true")
parser.add_argument('-y','--yellowBuoy',action = "store_true")
parser.add_argument('-r','--redTorpedo',action = "store_true")
parser.add_argument('-b','--blueTorpedo',action = "store_true")
parser.add_argument('-w','--wire',action = "store_true")
parser.add_argument('-e','--emperor',action = "store_true")
args = parser.parse_args()

# store initial value
init_north = shm.kalman.north.get()
init_east = shm.kalman.east.get()

w = shm.watchers.watcher()
w.watch(shm.kalman)
total_dist = 0

while (total_dist >= BEGIN_DIST_AWAY):
    w.wait()
    north_dist = shm.kalman.north.get() - init_north
    east_dist = shm.kalman.east.get() - init_east
    total_dist = math.sqrt(north_dist**2. + east_dist**2.)
    print "North: %.2f"  % north_dist
    print "East: %.2f" % east_dist

w.unwatch(shm.kalman)

if (args.orangeBuoy):
    csv_file = open('orange_buoy.csv','w')
    shm.buoy_settings.color.set("orange")
    shm.buoy_settings.enabled.set(1)
    w.watch(shm.orange_results)
elif (args.greenBuoy):
    csv_file = open('green_buoy.csv','w')
    shm.buoy_settings.color.set("green")
    shm.buoy_settings.enabled.set(1)
    w.watch(shm.green_results)
elif (args.yellowBuoy):
    csv_file = open('yellow_buoy.csv','w')
    shm.buoy_settings.color.set("yellow")
    shm.buoy_settings.enabled.set(1)
    w.watch(shm.yellow_results)
elif (args.redTorpedo or args.blueTorpedo):
    shm.nest_settings.enabled.set(1)
    csv_file = open('torpedo_raw.csv','w')
    if (args.redTorpedo):
        shm.nest_settings.color.set("red")
    else: 
        shm.nest_settings.color.set("blue")
    w.watch(shm.nest_results)
elif (args.wire):
    csv_file = open('wire_raw.csv','w')
    shm.wire_settings.enabled.set(1)
    w.watch(shm.wire_results)
elif (args.emperor):
    csv_file = open('emperor_raw.csv','w')
    shm.emperor_settings.enabled.set(1)
    w.watch(shm.emperor_results)

csvwriter = csv.writer(csv_file)

old_settings = termios.tcgetattr(sys.stdin)
try:
    tty.setcbreak(sys.stdin.fileno())

    while 1:
        print "Start watching area"
        w.wait()
        north_dist = shm.kalman.north.get() - init_north
        east_dist = shm.kalman.east.get() - init_east
        total_dist = math.sqrt(north_dist**2. + east_dist**2.)
        if (args.orangeBuoy):
            radius = math.sqrt(shm.orange_results.area.get()/math.pi)
        elif (args.greenBuoy):
            radius = math.sqrt(shm.green_results.area.get()/math.pi)
        elif (args.yellowBuoy):
            radius = math.sqrt(shm.yellow_results.area.get()/math.pi)
        elif (args.redTorpedo or args.blueTorpedo):
            tl_x = shm.nest_results.top_left_x.get()
            tl_y = shm.nest_results.top_left_y.get()
            tr_x = shm.nest_results.top_right_x.get()
            tr_y = shm.nest_results.top_right_y.get()
            bl_x = shm.nest_results.bottom_left_x.get()
            bl_y = shm.nest_results.bottom_left_y.get()
            br_x = shm.nest_results.bottom_right_x.get()
            br_y = shm.nest_results.bottom_right_y.get()
        elif (args.wire):
            bigger_height = shm.wire_results.biggest_height.get()
            bigger_width = shm.wire_results.biggest_width.get()
        elif (args.emperor):
            tl_x = shm.emperor_results.top_left_x.get()
            tl_y = shm.emperor_results.top_left_y.get()
            tr_x = shm.emperor_results.top_right_x.get()
            tr_y = shm.emperor_results.top_right_y.get()
            bl_x = shm.emperor_results.bottom_left_x.get()
            bl_y = shm.emperor_results.bottom_left_y.get()
            br_x = shm.emperor_results.bottom_right_x.get()
            br_y = shm.emperor_results.bottom_right_y.get()
        #Wire outputs
        if (args.wire):
            csv_row = [bigger_height, bigger_width, north_dist, east_dist, total_dist]
            print "Length/width: %d, Length/width: %d, North: %.2f, East: %.2f, Distance: %.2f" %(bigger_height, bigger_width, north_dist, east_dist, total_dist)
        #Torpedo and emperor outputs
        if (isSquare()):
            csv_row = [tl_x, tl_y, tr_x, tr_y, bl_x, bl_y, br_x, br_y, north_dist, east_dist, total_dist]
            print "tl_x: %d, tl_y: %d, tr_x: %d, tr_y: %d, bl_x: %d, bl_y: %d, br_x: %d, br_y: %d, north: %d, east: %d, total: %d" %(tl_x,tl_y,tr_x,tr_y,bl_x,bl_y,br_x,br_y, north_dist, east_dist, total_dist)
        #Buoy outputs
        if (isBuoy()):
            csv_row = [radius, total_dist]
            print "Radius: %d, Distance: %.2f" %(radius, total_dist)

        if (total_dist > .3):
            if (isSquare() and (abs(tl_y - bl_y) > 0)):#visisble
                csvwriter.writerow(csv_row)
            if (args.wire and (bigger_height > 0 and bigger_width > 0)):
                csvwriter.writerow(csv_row)
            if (isBuoy() and radius > 0):
                csvwriter.writerow(csv_row)

        if (total_dist >= END_DIST_AWAY):
            break
            
        if isData():
            if sys.stdin.read(1) == '\x1b':  # x1b is ESC
                break

finally:
        termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)
    
csv_file.close()

if (isBuoy()):
    if (args.orangeBuoy):
        obj = 'orange_buoy'
    elif (args.greenBuoy):
        obj = 'green_buoy'
    elif (args.yellowBuoy):
        obj = 'yellow_buoy'


elif (isRectangular()):
    if (args.redTorpedo or args.blueTorpedo):
        obj = 'torpedo'
    elif (args.emperor):
        obj = 'emperor'
    elif (args.wire):
        obj = 'wire'
    
    rect_data_analysis.RectDataAnalysis(obj)

eq_gen.EqGen(obj)


