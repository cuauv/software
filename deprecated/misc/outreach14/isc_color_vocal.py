#!/usr/bin/env python

# isc_bin_vocalization_demo.py
# Requires external library 'pyttsx'
# "Says" number(s) of vision-recognized bins
# Written for Ithaca Sciencenter Outreach, March 2014
# Last Updated 26 February 2014
# Questions? Email cwg46@cornell.edu

import pyttsx
import shm
from time import sleep
import threading
import random

ttsengine = pyttsx.init(debug=True)

voices = ttsengine.getProperty('voices')

index = random.randrange(len(voices))

index = 16

print('selected voice at index {}, {}'.format(index, voices[index].id))

ttsengine.setProperty('voice', voices[index].id)

def watch():
    lastcolor = '' 
    w = shm.watchers.watcher()
    shm.outreach.add_watcher(w)
    while True:
        w.wait()
        color = shm.outreach.color.get()
        if color:
            if lastcolor != color:
                ttsengine.say(color)
                lastcolor = color

t = threading.Thread(target=watch)
t.daemon = True
t.start()

ttsengine.startLoop(True)
