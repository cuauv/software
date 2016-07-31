#!/usr/bin/env python2

import sys
import pickle
import matplotlib.pyplot as plt
import matplotlib.cm as cmaps

pmap = pickle.load(open(sys.argv[1], 'r'))
dim = len(pmap)

xp, yp, zp = [], [], []

for x in range(dim):
    for y in range(dim):
        xp.append(x)
        yp.append(y)
        zp.append(pmap[x][y])

plt.scatter(xp, yp, c=zp, cmap=cmaps.coolwarm)
plt.show()
