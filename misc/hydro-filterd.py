#!/usr/bin/env python2.7

# Script that takes hydrophones_results and outputs filtered results to
# hydrophones_filtered.
# Should be started after hydrod.
#
# Ignores results when the ping period (ping_time) is not in
# [PERIOD_MIN, PERIOD_MAX].
#
# period will be set to the last accepted period.
# intensity will be set to the last accepted intensity (no filtering).
# heading will be set to the normalzed heading (see the comment on
# NONLINEARITY).
# uuid will be set to a random integer. It can be compared with previous values
# to see if the shm group has been updated.
#
# NONLINEARITY is currently disabled, because apparently it was a transducer
# problem.

import sys
import math
import time
import random
import json

import numpy as np

import shm
from misc.utils import watch_thread_wrapper

# Array of 2-tuples. First element in the tuple should be the hydrophone
# heading, the second element should be the corresponding normalized heading
# output. The array must be in ascending order.
# We use a naive bumpy normalization function whose derivatives are not
# continuous.
#NONLINEARITY = np.array(((-50, 0), (0, 90), (120, 180), (160, 270)))

PINGSIDE_COMPETITION = 1
PINGSIDE_PRACTICE = 2

t = lambda x: (x + 360) % 360

def hdvector(hd):
    return (math.sin(hd * 2*math.pi/360),
            math.cos(hd * 2*math.pi/360))

def resolve():
    px, py = shm.hydrophones_results.phaseX.get(), shm.hydrophones_results.phaseY.get()
    return t(shm.kalman.heading.get() + 180 - math.degrees(math.atan2(px + py, py)))

def hdata():
    return str(resolve()) + ' ' + str(shm.hydrophones_results.ping_time.get())

def resvector():
    return hdvector(resolve())


SIDE = PINGSIDE_COMPETITION

ENABLE_COLLECT = False
COLLECT_N = 10

collecting = ENABLE_COLLECT
collected = []
collected_n = 0

COLLECT_DANGLE = 30 * (2*math.pi)/360

# Guess that the initial heading is within MAX_DANGLE in front of us.
# Set to resvector() to instead use first read heading.

# XXX fix, don't weight this unduly

## OPTION 1: Blindly take current heading.
#initial = resvector()
#INITIAL_GUESS = math.atan2(initial[0], initial[1]) * 360/(2*math.pi)

## OPTION 2: Take first filtered heading.
INITIAL_GUESS = None

## OPTION 3: Use sub heading.
#INITIAL_GUESS = shm.kalman.heading.get()

PAST_HEADINGS_N = 4
PAST_HEADINGS = np.full((PAST_HEADINGS_N, 2), hdvector(INITIAL_GUESS) if INITIAL_GUESS is not None else (0, 0))
PAST_HEADINGS_I = 0

MAX_DANGLE = 30 * (2*math.pi)/360

def dot(a, b):
    return a[0] * b[0] + a[1] * b[1]

def dangle(a, b):
    if a == b:
        return 0
    else:
        return math.acos(dot(a, b))

last_pc = -1
oldvec = hdvector(INITIAL_GUESS) if INITIAL_GUESS is not None else (-1, -1)

def within(x, y, r):
    return abs(x - y) <= r

def loop(watcher, quit):
    global PAST_HEADINGS
    global PAST_HEADINGS_I
    global last_pc
    global oldvec
    global collecting
    global collected
    global collected_n
    global INITIAL_GUESS

    watcher.watch(shm.hydrophones_results)

    while not quit.is_set():
        watcher.wait()
        time.sleep(0.1)

        g = shm.hydrophones_results.get()

        if g.ping_count == last_pc:
            continue
        last_pc = g.ping_count

        print("raw heading from phasex before filtering " + hdata() + '; ' + str(g.ping_count))

        sideok = False

        if within(g.ping_time, 2000, 75):
            sideok = True
        elif SIDE == PINGSIDE_COMPETITION:
            sideok = within(g.ping_time, 1100, 75)
        elif SIDE == PINGSIDE_PRACTICE:
            sideok = within(g.ping_time, 900, 75)

        print("sideok: " + str(sideok))

        if not sideok:
            continue

        if collecting:
            print("Collecting, collected %d" % collected_n)
            collected.append(resvector())
            collected_n += 1

            if collected_n == COLLECT_N:
                s = (0, 0)
                for x in collected:
                    s = (s[0] + x[0], s[1] + x[1])

                s = (s[0] / math.sqrt(dot(s, s)), s[1] / math.sqrt(dot(s, s)))

                sp = (0, 0)
                for x in collected:
                    if dangle(s, x) <= COLLECT_DANGLE:
                        sp = (sp[0] + x[0], sp[1] + x[1])

                if sp[0] == 0 and sp[1] == 0:
                    print("Failed to collect enough correct data! Using average.")
                    sm = math.sqrt(dot(s, s))
                    sn = (s[0] / sm, s[1] / sm)
                    PAST_HEADINGS = np.full((PAST_HEADINGS_N, 2), sn)
                else:
                    sm = math.sqrt(dot(sp, sp))
                    sn = (sp[0] / sm, sp[1] / sm)
                    PAST_HEADINGS = np.full((PAST_HEADINGS_N, 2), sn)

                collecting = False

        if INITIAL_GUESS is None:
            initial = resvector()
            oldvec = initial
            INITIAL_GUESS = initial
            PAST_HEADINGS[0] = initial

        newvec = resvector()
        print(oldvec)
        print(newvec)
        print(dangle(newvec, oldvec))
        if dangle(newvec, oldvec) > MAX_DANGLE:
            print("diff too big")
            continue
        oldvec = newvec

        o = shm.hydrophones_filtered.get()

        o.count = g.ping_count
        o.period = g.ping_time
        o.intensity = g.intensity
        o.elevation = g.elevation

        PAST_HEADINGS[PAST_HEADINGS_I] = resvector()
        s = np.zeros((2))
        for i in range(0, PAST_HEADINGS_N):
            s += PAST_HEADINGS[i]
        o.heading = (math.atan2(s[0], s[1]) * 360/(2*math.pi) + 360) % 360
        PAST_HEADINGS_I = (PAST_HEADINGS_I + 1) % PAST_HEADINGS_N

        o.uuid = random.randint(-sys.maxsize - 1, sys.maxsize)

        print('send heading: {0}'.format(o.heading))

        shm.hydrophones_filtered.set(o)

random.seed()
watch_thread_wrapper(loop)
