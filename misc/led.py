#!/usr/bin/env python2

"""
LED routines.
Run with "auv-led <routine>"
"""

import sys
from time import sleep
from shm import led as s_led
from shm import watchers, switches

LEDS = {'port': {
            'red': s_led.port_red,
            'green': s_led.port_green,
            'blue': s_led.port_blue
                },
        'starboard': {
            'red': s_led.starboard_red,
            'green': s_led.starboard_green,
            'blue': s_led.starboard_blue
                },
        }

def set_all(value):
    for side in LEDS.values():
        for led in side.values():
            led.set(value)

def set_group(pins, val):
    [pin.set(val) for pin in pins]

def fade_group(pins, start, to, duration):
    delta = to - start
    timestep = duration / abs(delta)
    step = delta / abs(delta)

    cur_val = start
    set_group(pins, start)
    for i in range(abs(delta)):
        sleep(timestep)
        cur_val += step
        set_group(pins, cur_val)
    set_group(pins, to)

int1 = 0.03
int3 = 0.07
def blink(pins, c, val=255):
    for i in range(c):
        [pin.set(val) for pin in pins]
        sleep(int3)
        [pin.set(0) for pin in pins]
        sleep(int1)

def fade():
    while 1:
        fade_group(LEDS['port'].values(), 0, 255, 2)
        fade_group(LEDS['port'].values(), 255, 0, 2)
        fade_group(LEDS['starboard'].values(), 0, 255, 2)
        fade_group(LEDS['starboard'].values(), 255, 0, 2)

def flash():
    while 1:
        blink(LEDS['port'].values(), 2)
        sleep(int1)
        blink(LEDS['starboard'].values(), 2)
        sleep(int1)

def beacon():
    DEPTH_THRESH = 2.0
    while 1:
        for led in LEDS.values():
            blink([led], 3)
            blink([led], 4)
            blink([led], 5)

        if shared_vars["depth"].get() < DEPTH_THRESH:
            sleep(0.6)
        else:
            sleep(8) #Nobody can see the sub anyway

def police():
    int3 = 0.07
    LED1 = LEDS['port']['red']
    LED2 = LEDS['starboard']['blue']
    LEDS_USED = [LED1, LED2]
    def quint():
        blink(LEDS_USED, 5)

    while 1:
        for j in range(5):
            for i in range(2):
                blink([LED1],4)
                blink([LED2],4)
            for i in range(8):
                blink([LED1],1)
                blink([LED2],1)
        for j in range(3):
            blink(LEDS_USED, 5)
            sleep(int3 * 4)

def strobe():
    # == LED STROBE ==
    # TO BE USED IN EXTREME EMERGENCY CONDITONS ONLY
    # FOR MAXIMUM VISIBILITY (i.e. deadman breaching)
    int1 = 0.05
    int2 = 0.05
    while 1:
        set_all(255)
        sleep(int1)
        set_all(0)
        sleep(int2)

def daemon():
    watcher = watchers.watcher()
    watcher.watch(switches)

    last_sk = switches.soft_kill.get()
    while 1:
        watcher.wait(new_update=False)
        set_all(0)
        sk = switches.soft_kill.get()
        if sk and not last_sk:
            blink([LEDS["port"]["red"]], 3)
        if switches.hard_kill.get():
            while switches.hard_kill.get():
                LEDS["port"]["red"].set(255)
                sleep(0.5)
                LEDS["port"]["red"].set(0)
                set_group(LEDS["starboard"].values(), 255)
                sleep(0.5)
                set_group(LEDS["starboard"].values(), 0)
            blink([LEDS["port"]["green"], LEDS["starboard"]["green"]], 3)
        if not sk and last_sk:
            blink([LEDS["port"]["blue"], LEDS["starboard"]["blue"]], 3)
        last_sk = sk

options = { "beacon" : beacon, \
            "flash" : flash, \
            "off" : lambda: set_all(0), \
            "on" : lambda: set_all(255), \
            "police" : police, \
            "strobe" : strobe, \
            "daemon" : daemon, \
            "fade": fade }

if __name__ == "__main__":
    if len(sys.argv) != 2 or not options.has_key(sys.argv[1]):
        print "Please supply one argument:"
        print "\n".join(options.keys())
        sys.exit(1)

    try:
        options[sys.argv[1]]()
    except KeyboardInterrupt:
        print "done."
        set_all(0)
