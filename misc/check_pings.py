#!/usr/bin/env python3

import sys
import time

import shm

from mission.constants.region import PINGER_FREQUENCY
from misc.utils import watch_thread_wrapper

shm.hydrophones_settings.track_frequency_target.set(PINGER_FREQUENCY)
shm.hydrophones_settings.track_magnitude_threshold.set(600)
shm.hydrophones_settings.track_cooldown_samples.set(795000)

w = shm.watchers.watcher()
w.watch(shm.hydrophones_results_track)

# Expecting at least 3 pings in 7 seconds
PINGS_EXPECTED = 2
changes = 0
for i in range(70):
  if w.has_changed():
    changes += 1
    if changes >= PINGS_EXPECTED:
      break

  time.sleep(0.1)

sys.exit(not changes >= PINGS_EXPECTED)
