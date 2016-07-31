#!/usr/bin/env python2
import shm
import sys

if len(sys.argv) != 2:
    print "Usage: auv-lcd-set [screen]"
    print "[screen] should match a screen specified in the"
    print "screens folder. Specifying \"auto\" will enable"
    print "automatic dispatch."
    sys.exit(0)

# TODO: validation, listing of possible screens, auto-complete?

screentxt = str(sys.argv[1])
shm.lcd.screen.set(screentxt)
print "Setting LCD to \"%s\"" % screentxt

