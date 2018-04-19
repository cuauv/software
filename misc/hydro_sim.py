#!/usr/bin/env python3

import math
import time

import numpy as np

import misc.hydro2trans
import shm

from mission.framework.helpers import get_sub_position, get_sub_quaternion
from mission.constants.region import PINGER_FREQUENCY
from mission.constants.config import HYDROPHONES_PINGER_DEPTH

from misc.utils import register_exit_signals

PINGER_POSITION = np.array((10, 0, HYDROPHONES_PINGER_DEPTH))
PINGER_INTERVAL = 2.0

SOUND_SPEED = 1481.0

NIPPLE_DISTANCE = 0.013

PHASE2RATIO = SOUND_SPEED / (2 * math.pi * PINGER_FREQUENCY * NIPPLE_DISTANCE)

hydro_shm = shm.hydrophones_results_track

# Vector in body frame between first and second transducers.
trans_vec = np.array((1, 0, 0))
trans_vec2 = np.array((0, -1, 0))

start_time = time.time()
daemon_start_time = int(start_time)

# Turn off the default Python ^C handler that raises a KeyboardInterrupt.
# Now SIGINT should behave identically to SIGTERM: kill the program.
import signal
signal.signal(signal.SIGINT, signal.SIG_DFL)

while True:
  kalman = shm.kalman.get()

  sub_pos = get_sub_position(kalman)
  sub_quat = get_sub_quaternion(kalman)
  trans_deltas = (sub_quat * trans_vec, sub_quat * trans_vec2)

  ratios = misc.hydro2trans.h(PINGER_POSITION, sub_pos, trans_deltas)
  phases = [ratio / PHASE2RATIO for ratio in ratios]

  hydro_group = hydro_shm.get()
  hydro_group.diff_phase_x = phases[0]
  hydro_group.diff_phase_y = phases[1]
  hydro_group.ping_clock = 0 if hydro_group.ping_clock else 1
  hydro_group.daemon_start_time = daemon_start_time
  hydro_group.tracked_ping_time = time.time() - daemon_start_time
  hydro_shm.set(hydro_group)

  time.sleep(PINGER_INTERVAL - ((time.time() - start_time) % PINGER_INTERVAL))
