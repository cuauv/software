#!/usr/bin/env python2

import sys
import serial
import shm

if len(sys.argv) != 2:
    print "Usage: auv-lcd [port]"
    sys.exit(1)

port = sys.argv[1]
print "Connecting to LCD on " + port

ser = None

def conn():
    global ser
    ser = serial.Serial(port=port, baudrate=115200, timeout=1)

conn()

if ser.isOpen():
    print "Connected"
else:
    print "Error connecting"
    sys.exit(1)

last_pitch = 0
last_roll = 0
last_pressure = 0
startup = True

w = shm.watchers.watcher()
w.watch(shm.kalman) #pitch, roll
w.watch(shm.gpio) #pressure

def send(c, v):
    dig = int(abs(v))
    dec = int((abs(v) - dig)*100)
    if v < 0:
        dig *= -1

    if v < 0:
        dig -= 1
    dig += 127

    if dig >= 0 and dig <= 255 and dec >= 0 and dec <= 255:
        ser.write(chr(ord(c)))
        ser.write(chr(dig))
        ser.write(chr(dec))
        ser.read(1)

while True:
    np = round(shm.kalman.pitch.get(),2)
    nr = round(shm.kalman.roll.get(),2)
    nh = round(shm.pressure.internal.get(),2)
    if np != last_pitch or startup:
        last_pitch = np
        send('p', np)
    if nr != last_roll or startup:
        last_roll = nr
        send('r', nr)
    if nh != last_pressure or startup:
        last_pressure = nh
        send('h', nh)
    startup = False
    w.wait(False)
