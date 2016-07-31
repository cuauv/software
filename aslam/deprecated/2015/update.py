#!/usr/bin/env python3.4

from model import slam, objects

import shm

while 1:
  s = shm.slam.get()

  for obj in objects:
    pos = [x['realr'] for x in slam.estimate(obj)['estimated']['vectorr']]
    setattr(s, obj + '_n', pos[0])
    setattr(s, obj + '_e', pos[1])
    setattr(s, obj + '_d', pos[2])

  shm.slam.set(s)
  
  print('SHM updated!')
