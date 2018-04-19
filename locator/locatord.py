#!/usr/bin/env python2
'''
Runs a daemon that runs locators on buoys.

Use shm.locator#_settings to enable the locators and to
set which colors, etc. they're looking for
'''

import locator
import shm
import pickle
import argparse

parser = argparse.ArgumentParser(description="locator test script")
parser.add_argument('-g', '--gui',
                    dest="gui",
                    action="store_const",
                    default=False,
                    const=True,
                    help="whether to display debug output")
args = parser.parse_args()

kill = False


# Begin with uninitialized  locators
# We'll initialize these when they're enabled
locator1 = None
locator2 = None

watcher = shm.watchers.watcher()
watcher.watch(shm.kalman)
watcher.watch(shm.locator1_settings)
watcher.watch(shm.locator2_settings)

#Handle interrupts so that this daemon can die peacefully
import signal
def signal_handler(signal, frame):
    global kill
    print "INTERRUPT"
    watcher.broadcast()
    kill = True

signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)

def get_layout():
    try:
        state = pickle.loads(shm.layout.state.get())
    except:
        print "Invalid mission layout state! Cannot initialize buoy positions."
        print "You might need to start running a mission log."
        raise

    #Gather buoy positions
    n_off = state[0]
    e_off = state[1]
    positions = state[2]

    out = [(positions[2*i,0]-n_off, positions[2*i+1,0]-e_off) for i in range(2,5)]
    return out

   
def start_locator1():
    try:
        orange,yellow,green = get_layout()
    except:
        print "Disabling locator 1 - need layout"
        shm.locator1_settings.enabled.set(0)
        return None
    else:
        return locator.LocatorRunner(
                                shm.locator1_settings.color.get(),
                                orange_pos=orange,
                                #green_pos=green,
                                #yellow_pos=yellow,
                                led_pos=green, #TODO: make this its own thing?
                                prior_sigma=1.0,
                                display=args.gui,
                                locator_var=shm.locator1,
                                settings_var=shm.locator1_settings)

def start_locator2():
    try:
        orange,yellow,green = get_layout()
    except:
        print "Disabling locator 2 - need layout"
        shm.locator2_settings.enabled.set(0)
        return None
    else:
        return locator.LocatorRunner(
                                shm.locator2_settings.color.get(),
                                orange_pos=orange,
                                #green_pos=green,
                                #yellow_pos=yellow,
                                led_pos=green, #TODO: make this its own thing?
                                prior_sigma=1.0,
                                display=args.gui,
                                locator_var=shm.locator2,
                                settings_var=shm.locator2_settings)

def gogogo():
    global locator1, locator2
    while not kill:
        if shm.locator1_settings.reset.get():
            print "(Re)starting Locator 1"
            shm.locator1_settings.reset.set(0)
            locator1 = start_locator1()

        elif locator1:
            if shm.locator1_settings.enabled.get():
                locator1.update()
            else:
                print "Stopping Locator 1"
                locator1 = None
        elif shm.locator1_settings.enabled.get() and shm.locator1_settings.color.get():
            print "Starting Locator 1"
            locator1 = start_locator1()

        if shm.locator2_settings.reset.get():
            print "(Re)starting locator 2"
            shm.locator2_settings.reset.set(0)
            locator2 = start_locator2()
        elif locator2:
            if shm.locator2_settings.enabled.get():
                locator2.update()
            else:
                print "Stopping locator 2"
                locator2 = None
        elif shm.locator2_settings.enabled.get() and shm.locator2_settings.color.get():
            print "Starting Locator 2"
            locator2  = start_locator2()

        watcher.wait()

import threading
t1 = threading.Thread(target=gogogo)
t1.start()

from time import sleep
while not kill:
    sleep(0.05)
