#!/usr/bin/env python3

import shm

import sys, tty, termios

def get_char():
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return ch

def sinc(var, amount=1):
    print(var.get())
    var.set(var.get() + amount)
    print(var.get())
    print()

desires = shm.navigation_desires

while True:
    c = get_char()
    print(c)

    if c == 'q':
        sys.exit()
    elif c == 'z':
        desires.depth.set(shm.kalman.depth.get())
        desires.heading.set(shm.kalman.heading.get())
        desires.pitch.set(shm.kalman.pitch.get())
        desires.roll.set(shm.kalman.roll.get())
        desires.speed.set(0)
        desires.sway_speed.set(0)
    elif c == 's':
        shm.switches.soft_kill.set(1)
    elif c == 'u':
        sinc(desires.depth, -.1)
    elif c == 'd':
        sinc(desires.depth, .1)
    elif c == 'r':
        sinc(desires.heading, 5)
    elif c == 'l':
        sinc(desires.heading, -5)
    elif c == '0':
        desires.speed.set(0)
    elif c == '1':
        desires.speed.set(.1)
    elif c == '2':
        desires.speed.set(.2)
    elif c == '3':
        desires.speed.set(.4)
    elif c == '4':
        desires.speed.set(.7)
    elif c == '5':
        desires.speed.set(-.1)
    elif c == '6':
        desires.speed.set(-.2)
    elif c == '7':
        desires.speed.set(-.4)
    elif c == '8':
        desires.speed.set(-.7)
        
    
    
