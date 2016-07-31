#!/usr/bin/env python2
from mission.led_status import LED
from random import randint

alpha = "abcdefghijklmnopqrstuvqxyz"
speed = 0.16

def msg(x):
    print "-" * len(x)
    print x
    print "-" * len(x)

       

def randl():
    return alpha[randint(0, len(alpha)-1)]

def play():
    raw_input("Push [enter] to start flashing a letter")
    print "Flashing a letter now..."
    l = randl()
    LED(l, "blue", rate=speed, blocking=True)
    gs = raw_input("What letter was that?\r\n>")
    if gs.lower() == l:
        print "YOU ARE RIGHT!"
    else:
        print "Wrong, it was a " + str(l).upper()

msg("Welcome to the CUAUV Morse Code Trainer Game!")

try:
    while True:
        play()
except KeyboardInterrupt:
    pass

print "\r\n"
msg("Goodbye!")
