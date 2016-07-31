#!/usr/bin/env python2

from interface import *
import matplotlib.pyplot as plt
from matplotlib import cm

plt.rcParams['interactive'] = True

plt.xlim(0,50)
plt.ylim(0,50)

objects = []

def draw():
    plt.clf()
    pts = []
    for r in range(len(objects[0].pmap)):
        for c in range(len(objects[0].pmap[0])):
            if (r % 2 == 0) and (c % 2 == 0):
            #if True:
                m = 0.
                for o in objects:
                    if o.pmap[r][c] > m: m = o.pmap[r][c]
                pts.append((r*0.1, c*0.1, m*1e6))
    plt.scatter([p[0] for p in pts], [p[1] for p in pts], c=[p[2] for p in pts], cmap=cm.coolwarm)
    plt.scatter([o.centroid[0] for o in objects], [o.centroid[1] for o in objects], c='k')
    plt.draw() 
    raw_input('Press \'Enter\' to continue.')

l = Layout('tagged_teagle_alt.layout')

objects = l.objects.values()

draw()

l.set_start_position(5, 5, 'start')

objects = l.objects.values()

draw()

l.update_position('start', 6, 6, 0.5)
objects = l.objects.values()
draw()
