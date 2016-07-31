#!/usr/bin/env python2

'''
Script to display the sub's position and where it guesses the
pinger is based off of hydrophone data.

It assumes that the floor is a flat plane and uses the altitude of the sub
along with the ping heading and 'elevation' angle to guess the point of
the pinger.
We assume that the pinger is on the ground and hence 'altitude' meters down.
Then draw the line in the direction the hydrophones give and stop when
it hits the ground.

The sub is plotted in red line and the pinger in blue dots.
'''

import shm
import pylab
from numpy import cos, sin, radians, tan

pylab.ion() # interactive pylab session for live updates
figure = pylab.figure()

n = shm.kalman.north.get()
e = shm.kalman.east.get()
sub_pos = pylab.plot([e], [n], "r-")[0] #Plot sub position over time as red line
points = pylab.plot([],[], "bo")[0] # Plot pinger points as blue dots

SIZE = 10 # half the width of the area to display, in meters
pylab.xlim( (e-SIZE,e+SIZE) )
pylab.ylim( (n-SIZE,n+SIZE) )

w = shm.watchers.watcher()
w.watch(shm.hydrophones_results)

pylab.draw()

while(True):
    n = shm.kalman.north.get()
    e = shm.kalman.east.get()

    hdg = shm.kalman.heading.get()
    diff = shm.hydrophones_results.heading.get()
    elevation = shm.hydrophones_results.elevation.get()
    altitude = shm.dvl.savg_altitude.get()

    # Trig to estimate pinger point
    target_n = n + cos(radians(hdg + diff))*altitude *tan(radians(elevation))
    target_e = e + sin(radians(hdg + diff))*altitude * tan(radians(elevation))

    # Drawing
    xs, ys = sub_pos.get_data()
    sub_pos.set_data(list(xs)+[e],list(ys)+[n])

    if abs(target_n) < 1e3 and abs(target_e) < 1e3:
        # Plot only reasonable points
        # Too large values come from a nearly 90 degree elevation
        # which sends things waaaay off into the land of magnitude 1e16
        xs,ys = points.get_data()
        points.set_data(list(xs)+[target_e], list(ys)+[target_n])
    figure.canvas.draw()

    w.wait()
