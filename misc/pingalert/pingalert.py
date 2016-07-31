#!/usr/bin/env python2

#Ping alert code by Jeff to play a sound when the sub appears

import datetime
import os
import socket
import sys
import time

import pygame

if len(sys.argv) > 1 and sys.argv[1] == "mini":
    subaddress = "192.168.0.91"
else:
    subaddress = "192.168.0.93"

s = socket.socket()
s.settimeout(0.15)

pygame.init()

def alert():
    print("SUB FOUND! ALERT ALERT ALERT!")
    pingalert_dir = os.path.dirname(os.path.realpath(__file__))
    pygame.mixer.music.load(os.path.join(pingalert_dir, "sound.wav"))
    pygame.mixer.music.play()
    time.sleep(3)
    exit()

while 1:
    try:
        s.connect((subaddress, 22))
    except socket.error:
        now = datetime.datetime.now()
        print("Nope, not there @ " + now.strftime("%H:%M:%S"))
    else:
        alert()
    time.sleep(0.5)
