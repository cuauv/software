#!/usr/bin/env python2

"""
Watch for hydrophones derp, and reset it
"""

from time import sleep
from shm import hydrophones_results as hr
from shm import sensor_power as sp
from shm import watchers

watcher = watchers.watcher()
watcher.watch(hr)

last_ping_count = hr.ping_count.get()
num_bad_pings = 0

while True:
    watcher.wait(new_update=False)
    if hr.ping_count.get() != last_ping_count:
        last_ping_count = hr.ping_count.get()
        if hr.ping_time.get() < 400:
            num_bad_pings = num_bad_pings + 1
            if num_bad_pings >= 3:
                print "Resetting hydrophones"
                sp.enable_12v_6.set(0)
                sleep(0.1)
                sp.enable_12v_6.set(1)
                num_bad_pings = 0
        else:
            num_bad_pings = 0
