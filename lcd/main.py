#!/usr/bin/env python2
import argparse
import cairo
import sys, signal
from time import time, sleep

from screendispatcher import ScreenDispatcher
from screens.exitscreen import ExitScreen

ap = argparse.ArgumentParser(description='AUV LCD Daemon')
ap.add_argument('--virtual', action='store_const', const=True, help='Enable virtual testing mode (gtk LCD display)')
args = ap.parse_args()

if args.virtual:
    # Use GTK lcd screen window instead of hardware LCD
    from virtual_lcd import VirtualLcd
    screen = VirtualLcd()
else:
    # Connect to the hardware LCD
    from lcd import Lcd
    screen = Lcd()

screen.init()

dispatch = ScreenDispatcher()

def interrupt_handler(signal, frame):
    print "\nExiting"
    dispatch.stop_all()
    surface, cr = screen.new_surface()
    exitscreen = ExitScreen()
    exitscreen.draw(cr)
    screen.write_surface(surface)
    sys.exit(0)

signal.signal(signal.SIGINT, interrupt_handler)
signal.signal(signal.SIGTERM, interrupt_handler)

FPS_LIMIT = 25

while True:
    start = time()
    surface, cr = screen.new_surface()
    dispatch.draw(cr)
    screen.write_surface(surface)
    remaining = (1.0 / FPS_LIMIT) - (time() - start)
    if remaining > 0:
        sleep(remaining)
