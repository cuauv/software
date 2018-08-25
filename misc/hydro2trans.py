#!/usr/bin/env python3

import functools
import math

import numpy as np

import shm

from scipy.optimize import minimize

from misc.utils import watch_thread_wrapper
from mission.framework.helpers import get_sub_position, get_sub_quaternion
from mission.framework.primitive import NoOp

from conf.vehicle import VEHICLE

PINGER_FREQ = 39500
TRACK_MAG_THRESH = 11000

SOUND_SPEED = 1481.0

# Distance (perpendicular) between the 'phones.
NIPPLE_DISTANCE = 0.0178

# Directional vector in body frame between first and second transducers.
trans_vec = np.array((1, 0, 0))
trans_vec2 = np.array((0, -1, 0))

is_mainsub = VEHICLE == 'castor'

class Localizer:
  def __init__(self, frequency):
    self.observations = []
    self.PHASE2RATIO = SOUND_SPEED / (2 * math.pi * frequency * NIPPLE_DISTANCE)

  # Returns whether the phases are physically possible given the frequency,
  # NIPPLE_DISTANCE, and speed of sound in water
  def is_valid(self, phase_x, phase_y):
    kx = self.PHASE2RATIO * phase_x
    ky = self.PHASE2RATIO * phase_y

    return kx * kx + ky * ky <= 1.0

  def get_heading_elevation(self, phase_x, phase_y):
    kx = self.PHASE2RATIO * phase_x
    ky = self.PHASE2RATIO * phase_y
    kz_2 = 1 - kx * kx - ky * ky
    if kz_2 < 0:
      kz_2 = 0

    heading = (3.14 if is_mainsub else 0) + math.atan2(ky, kx) # Oopsy daisys, I did it againsy!
    # > hi every1 im new!!!!!!!
    # > holds up spork my name is katy but u can call me t3h PeNgU1N oF d00m!!!!!!!!
    # > lol?as u can see im very random!!!!
    # > thats why i came here, 2 meet random ppl like me _? im 13 years old (im mature 4 my age tho!!)
    # > i like 2 watch invader zim w/ my girlfreind (im bi if u dont like it deal w/it) its our favorite tv show!!!
    # > bcuz its SOOOO random!!!! shes random 2 of course but i want 2 meet more random ppl =)
    # > like they say the more the merrier!!!! lol?neways i hope 2 make alot of freinds here so give me lots of commentses!!!!

    #
    # > DOOOOOMMMM!!!!!!!!!!!!!!!! <--- me bein random again _^ hehe?toodles!!!!!

    # > love and waffles,

    # > t3h PeNgU1N oF d00m
    # ~~~~~~~~~
    elevation = math.acos(math.sqrt(kz_2))
    return -math.degrees(heading), math.degrees(elevation)

  def add_observation(self, phases, sub_pos, sub_quat):
    trans_deltas = (sub_quat * trans_vec, sub_quat * trans_vec2)
    ks = [phase * self.PHASE2RATIO for phase in phases]
    print("Observation: ", ks)
    self.observations.append((sub_pos, trans_deltas, ks))

  def get_score(self, guess, use_all=False):
    total_error = 0
    for sub_pos, transducer_deltas, ks in self.observations:
      exp_k_x, exp_k_y = h(guess, sub_pos, transducer_deltas)
      error = exp_k_x - ks[0]
      total_error += error * error
      if use_all:
        error = exp_k_y - ks[1]
        total_error += error * error

    return total_error

  def compute_position(self):
    check_both = functools.partial(self.get_score, use_all=True)

    bounds = [(-50, 50), (-50, 50), (1, 3)]
    res2 = minimize(check_both, np.array((0, 0, 0)), bounds=bounds)

    return res2.x

# This function takes a state (pinger and sub position)
# and provides an expected observation. (py / d)
def h(pinger_position, sub_position, transducer_deltas):
  pos_delta = pinger_position - sub_position

  comp = pos_delta.dot(transducer_deltas[0])
  comp2 = pos_delta.dot(transducer_deltas[1])

  d = np.linalg.norm(pos_delta)

  return comp / d, comp2 / d

if __name__ == "__main__":
  def func(watcher, quit_event):
    watcher.watch(shm.hydrophones_results_track)

    observations = []

    localizer = Localizer(shm.hydrophones_settings.track_frequency_target.get())

    while not quit_event.is_set():
      # input()
      results = shm.hydrophones_results_track.get()
      kalman = shm.kalman.get()

      sub_pos = get_sub_position(kalman)
      sub_quat = get_sub_quaternion(kalman)

      print("------------------------------------------------------")
      localizer.add_observation((results.diff_phase_x, results.diff_phase_y), sub_pos, sub_quat)

      pinger_pos, pinger_pos_all = localizer.compute_position()

      print("%d observations. current position: %0.2f %0.2f %0.2f" % \
            (len(localizer.observations), sub_pos[0], sub_pos[1], sub_pos[2]))
      print("Predicted (2 Trans): %0.2f %0.2f %0.2f" % \
            (pinger_pos[0], pinger_pos[1], pinger_pos[2]))
      print("Predicted (3 Trans): %0.2f %0.2f %0.2f" % \
            (pinger_pos_all[0], pinger_pos_all[1], pinger_pos_all[2]))

      watcher.wait(new_update=False)

  shm.hydrophones_settings.track_frequency_target.set(PINGER_FREQ)
  shm.hydrophones_settings.track_magnitude_threshold.set(TRACK_MAG_THRESH)
  watch_thread_wrapper(func)
