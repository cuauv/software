#!/usr/bin/env python3

import sys
import time

import shm

from misc.utils import watch_thread_wrapper

w = shm.watchers.watcher()
w.watch(shm.desires)

changes = 0
for i in range(30):
  if w.has_changed():
    changes += 1

  time.sleep(0.1)

sys.exit(not changes > 15)
