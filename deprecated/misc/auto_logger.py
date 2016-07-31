#!/usr/bin/env python2
"""
Starts a new shmlog every time the vehicle enters the water.
Alex Spitzer 2014
"""

import subprocess
import shm
from misc.utils import watch_thread_wrapper

LOG_CMD = "auv-shmlogd"

def uptime_watcher(w, quit_event):
    w.watch(shm.uptime)
    log_process = None
    while not quit_event.is_set():
        in_water = shm.uptime.in_water.get()
        if in_water and log_process is None:
            log_process = subprocess.Popen(LOG_CMD)

        elif not in_water and log_process is not None:
            log_process.terminate()
            log_process = None

        w.wait(False)

watch_thread_wrapper(uptime_watcher)
