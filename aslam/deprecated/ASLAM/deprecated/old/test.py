#!/usr/bin/env python

import matplotlib.pyplot as plt
from classes import *
import math
from matplotlib import cm
from numpy import random
plt.rcParams['interactive'] = True

plt.xlim(0,50)
plt.ylim(0,50)

def draw():
    plt.clf()
    pts = []
    for r in range(len(objects[0].pmap)):
        for c in range(len(objects[0].pmap[0])):
            if (r % 5 == 0) and (c % 5 == 0):
                m = 0.
                for o in objects:
                    if o.pmap[r][c] > m: m = o.pmap[r][c]
                pts.append((r*0.1, c*0.1, m*1e6))
    plt.scatter([p[0] for p in pts], [p[1] for p in pts], c=[p[2] for p in pts], cmap=cm.coolwarm)
    plt.draw() 
    input('Press \'Enter\' to continue.')

l = Localizer()

o1 = Object((10,10), 1)
o2 = Object((20,20), 1)
o3 = Object((10,30), 1)

objects = [o1, o2, o3]
#objects = [o1]

draw()

l.update(movements = [Movement(o1, (5,5), 0.1, None)])

draw()

l.update(observations = [Observation(o1, o2, None, None, 20, 10, None)])

draw()

l.update(observations = [Observation(o1, o2, math.sqrt(200), 1, None, None, None)])

draw()
