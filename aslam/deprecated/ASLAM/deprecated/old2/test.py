#!/usr/bin/env python2.7

from classes import *
import numpy as n

S = State(5, 1, 5, 1)

for x in range(5): S.objects[str(x)] = Object()

S.update()
S.hObs('0', 45, 5)
S.dObs('0', 10**(1./2), 0.5)
S.update()

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure()
ax = fig.add_subplot(111, projection = '3d')

pmap = S.objects['0'].pmap
xv, yv, zv = [], [], []
for x in range(len(pmap)):
    for y in range(len(pmap[0])):
        xv += [x / GRIDSCALE]
        yv += [y / GRIDSCALE]
        zv += [pmap[x][y]]
ax.scatter(xv, yv, zv)
plt.show()
#for i in range(len(x)):
#    for j in range(len(y)):

