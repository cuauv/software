#!/usr/bin/env python3

import shm

def set_zero_heading():
    prev_offset = shm.kalman_settings.heading_offset.get()
    new_offset = (prev_offset - shm.kalman.heading.get()) % 360
    shm.kalman_settings.heading_offset.set(new_offset)

    prev_desire = shm.navigation_desires.heading.get()
    new_desire = (prev_desire + (new_offset - prev_offset)) % 360
    shm.navigation_desires.heading.set(new_desire)
