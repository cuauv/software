#!/usr/bin/env python

import matplotlib.pyplot as plt
from classes import *
import math
from matplotlib import cm
import random
plt.rcParams['interactive'] = True

plt.xlim(0,50)
plt.ylim(0,50)

def draw():
    plt.clf()
    pts = []
    for r in range(len(objects[0].pmap)):
        for c in range(len(objects[0].pmap[0])):
            if (r % 2 == 0) and (c % 2 == 0):
                m = 0.
                for o in objects:
                    if o.pmap[r][c] > m: m = o.pmap[r][c]
                pts.append((r*0.1, c*0.1, m*1e6))
    plt.scatter([p[0] for p in pts], [p[1] for p in pts], c=[p[2] for p in pts], cmap=cm.coolwarm)
    plt.draw() 
    plt.pause(0.01)

l = Localizer()

robj = ((10, 10), (14, 17), (10, 15), (5, 3), (2, 14))

objects = []
for o in robj:
    objects.append(Object((o[0] + (2*random.random() - 1), o[1] + (2*random.random() - 1)), 1))

objects[0] = Object((robj[0][0], robj[0][1]), 1)

def pd():
    sm = 0
    for i in range(len(objects)):
        if i == 1: print(((robj[i][0] - objects[i].centroid[0]), (robj[i][1] - objects[i].centroid[1])))
        sm += math.sqrt((robj[i][0] - objects[i].centroid[0])**2 + (robj[i][1] - objects[i].centroid[1])**2)
    print(sm)
#    input('Press \'Enter\' to continue.')

draw()
pd()

i = 0
while True:
    i += 1
    if i % 10 != 0:
        n = 1
        #n = random.randint(1, len(objects) - 1)
        dist, heading = toPolar(robj[n][0] - robj[0][0], robj[n][1] - robj[0][1])
        l.update(observations = [Observation(objects[0], objects[n], random.gauss(dist, 1), 1, random.gauss(heading, 5), 5, None)])
    else:
        pass
        #print('Bad reading')
        #dist, heading = toPolar(robj[n][0] - robj[0][0], robj[n][1] - robj[0][1])
        #l.update(observations = [Observation(objects[0], objects[n], random.gauss(dist, 5), 1, random.gauss(heading, 25), 5, None)])
    draw()
    pd()
