#!/usr/bin/env python3.4

MIN_PERIOD, MAX_PERIOD, MAX_ELEVATION = 1800, 2200, 80

from model import slam
from aslam.repr import *
from aslam.parameter import *

import shm, math, time, random

def t(h):
  h = (h + 360) % 360
  h = h - 360 if h > 180 else h
  return h

def heading():
  px, py = shm.hydrophones_results.phaseX.get(), shm.hydrophones_results.phaseY.get()
  return t(-180 - math.degrees(math.atan2(px + py, py)))

count = shm.hydrophones_results.ping_count.get()

while 1:
  # rpos = [shm.kalman.north.get(), shm.kalman.east.get(), shm.kalman.depth.get()]
  rpos = [0., 0., 0.]
  pos = vectorr([realr(x) for x in rpos])
  n_count = shm.hydrophones_results.ping_count.get()

  if n_count > count or 1:
    n_count = count

    period, elevation, h = shm.hydrophones_results.ping_time.get(), shm.hydrophones_results.elevation.get(), heading()

    h = h + random.random() * 60 + time.time()
    h = t(h)

    #if period < MIN_PERIOD or period > MAX_PERIOD or elevation > MAX_ELEVATION:
      #print('Ignoring ping: period {0}, elevation {1}, heading {2}.'.format(period, elevation, h))
      #continue  

    slam.observe('heading', {'x': pactual(pos), 'y': pobject('octagon_1')}, realr(math.radians(h)), 0.5)
    octpos = [x['realr'] for x in slam.estimate('octagon_1')['estimated']['vectorr']]
    heading_recalc = math.degrees(math.atan2(octpos[1] - rpos[1], octpos[0] - rpos[0]))
    
    shm.hydrophones_filtered.heading.set(heading_recalc)
    shm.hydrophones_filtered.count.set(shm.hydrophones_filtered.count.get() + 1)
    # shm.hydrophones_filtered.elevation.set(elevation)

    print('Heading: {0} OctPos: {1} Recalc Heading: {2}'.format(h, octpos, heading_recalc))
