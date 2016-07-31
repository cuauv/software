#!/usr/bin/env python2
import shm
from misc.utils import watch_thread_wrapper

'''
Displays mission messages from shared memory (useful for playback)
Jeff Heidel 2012
'''

def missionPrinter(watcher, quit_event):
    watcher.watch(shm.mission)
    current = ""
    while not quit_event.is_set():
        new = shm.mission.task_messages.get()
        if new != current:
            current = new
            print current

        watcher.wait()

watch_thread_wrapper(missionPrinter)
