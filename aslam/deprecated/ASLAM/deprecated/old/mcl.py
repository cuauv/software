#!/usr/bin/env python3

# Basic MCL
from time import sleep
import random
import matplotlib; matplotlib.use('QT4Agg')
import matplotlib.pyplot as plt
plt.rcParams['interactive'] = True

GRID_X = 20.
GRID_Y = 20.
NUM_PARTICLES = 1000
UNCERTAINTY = 0.2
ROLLOVER_FRAC = 0.9

particles = [(random.random() * GRID_X, random.random() * GRID_Y, 1.) for i in range(NUM_PARTICLES)]

def move(dx, dy):
    for i in range(len(particles)): particles[i] = (particles[i][0] + dx, particles[i][1] + dy, particles[i][2])

def update(pfxn):
    for i in range(len(particles)): particles[i] = (particles[i][0], particles[i][1], particles[i][2] * pfxn(particles[i][0], particles[i][1]))

def regenerate():
    global particles
    nparticles = []
    for i in particles:
        #while(len(nparticles) < NUM_PARTICLES):
            if (random.random() * i[2] > (1 - ROLLOVER_FRAC)) and len(nparticles) < NUM_PARTICLES:
                for n in range(0, int(round(i[2]))): nparticles.append((i[0] + random.uniform(-UNCERTAINTY, UNCERTAINTY), i[1] + random.uniform(-UNCERTAINTY, UNCERTAINTY), i[2]))
    while(len(nparticles) < NUM_PARTICLES):
        nparticles.append((random.random() * GRID_X, random.random() * GRID_Y, 1.))
    particles = nparticles
        
def disp():
    plt.clf()
    plt.plot([p[0] for p in particles], [p[1] for p in particles], 'bo')
    plt.xlim(0, GRID_X)
    plt.ylim(0, GRID_Y)
    plt.draw()

def tfxn(x, y):
    if 9 < x < 10 and 9 < y < 10:
        return 10.
    else:
        return 0.1

if __name__ == '__main__':
    #plt.ion()
    disp()
    it = 0
    while True:
        plt.pause(0.1)
        update(tfxn)
        move(0.2, 0.2)
        regenerate()
        #move(0.2, 0.2)
        disp()
        it += 1
