#!/usr/bin/env python2

'''
3D Position/dPosition Tracker
Christopher Goes, 2014
'''

import shm
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import time
import numpy as n

dt = 0.1

fig = plt.figure()
#ax = fig.add_subplot(111, axes='3d')
ax= fig.gca(projection='3d')
plt.ion()

pos = lambda: (shm.kalman.north.get(), shm.kalman.east.get(), -shm.kalman.depth.get())

past_pos = pos()

x, y, z = [], [], []

while True:
    cpos = pos()
    map(lambda (x, y):globals()[x].append(y), zip(['x', 'y', 'z'], cpos))
    ax.plot(n.array(x[-2:]), n.array(y[-2:]), n.array(z[-2:]))
    delta_pos = map(lambda (x,y):x-y, zip(cpos, past_pos))
    past_pos = cpos
    dx, dy, dz = delta_pos
    ax.plot(n.array([x[-1], x[-1] + dx/dt]), n.array([y[-1], y[-1] + dy/dt]), n.array([z[-1], z[-1] + dz/dt]))
    plt.draw()
    plt.pause(dt)

