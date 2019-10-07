#!/usr/bin/env python3

#Script for drawing polar plots of the heading and elevation of pings. Read Hydrophones Code wiki entry.

import math
import shm
import matplotlib.pyplot as plt

REFRESH_INTERV = 0.25 #heading plot updates at this interval (in seconds)

#initializing the plot window
fig = plt.figure(figsize = (7, 7))
ax = fig.add_subplot(111, polar = True)

plt.title("Heading and Elevation")

#settings the axes properties and removing ticks
ax.set_yticklabels([])
ax.set_theta_zero_location('N')
ax.set_theta_direction(-1)
ax.set_ylim(0, 1) #radius of plot is 1

#initializing the graph with arbitrary numbers
theta = [0, 0]
r = [0, 1]
(line, ) = ax.plot(theta, r)

while 1:
	#retreiving the most recent data from shm
	theta = math.radians(shm.hydrophones_results_track.tracked_ping_heading.get())
	r = math.cos(math.radians(shm.hydrophones_results_track.tracked_ping_elevation.get()))

	#updating the graph
	line.set_xdata([theta, theta])
	line.set_ydata([0, r])

	#drawing the plot and pausing until the next update
	plt.draw()
	plt.pause(REFRESH_INTERV)


