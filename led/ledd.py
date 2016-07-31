#!/usr/bin/env python

import shm
import time
import argparse
from misc.utils import watch_thread_wrapper

import util

parser = argparse.ArgumentParser(description = 'CUAUV LED Daemon')
args = parser.parse_args()

util.set_all(util.colors.none)

@watch_thread_wrapper
def run(watcher, quit):
  watcher.watch(shm.switches)
  watcher.watch(shm.merge_status)
  while not quit.is_set():
    watcher.wait()
    switches      = shm.switches.get()
    merge_status  = shm.merge_status.get()
    if switches.hard_kill:
      util.set_all(util.colors.red)
    elif switches.soft_kill:
      util.set_all(util.colors.yellow)
    elif merge_status.mission_start:
      util.set_all(util.colors.green)
    else:
      util.set_all(util.colors.none)
