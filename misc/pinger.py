#!/usr/bin/env python3

import shm
import time

last_ping = 0
t = time.time()

while True:
    new_ping = shm.trax.heading_status.get()
    new_time = time.time()

    if new_ping != last_ping:
        last_ping = new_ping
        t = new_time

    if new_time - t > 5:
        shm.leds.starboard_color_red.set(255)
        shm.leds.starboard_color_green.set(0)
        shm.leds.starboard_color_blue.set(0)
        shm.leds.port_color_red.set(255)
        shm.leds.port_color_green.set(0)
        shm.leds.port_color_blue.set(0)
    else:
        shm.leds.starboard_color_red.set(0)
        shm.leds.starboard_color_green.set(255)
        shm.leds.starboard_color_blue.set(0)
        shm.leds.port_color_red.set(0)
        shm.leds.port_color_green.set(255)
        shm.leds.port_color_blue.set(0)
