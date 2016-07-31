#!/usr/bin/env python2

'''
This script sends out the USD reset packet to the specified port

You must know the port number and the baud of the device you want to reset
Bauds are found in the config files and ports can be found in seriald logs

It may take a few tries to get the packet to the board with the serial daemon
talking over us. This functionality would be better placed in the serial daemon.
'''

import sys
import struct
import serial

SUB = "thor"
PORT_FORMAT = "/dev/ttyUSB_" + SUB + "_%s"

RESET_PACKET = 0xAA

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print "Usage:\tpython reset_device.py [port number] [baud]"
        print "\te.g. python reset_device.py 3 57600 sends the reset packet",
        print "to %s at a baud of 57600" % PORT_FORMAT % 3
        sys.exit(1)

    try:
        baud = int(sys.argv[2])
    except ValueError:
        print "Error: Invalid baud"
        sys.exit(1)

    port = PORT_FORMAT % sys.argv[1]
    try:
        s = serial.Serial(port, baud)
    except serial.serialutil.SerialException:
        print "Error: Could not find port %s" % port
        sys.exit(1)

    s.write(struct.pack("BBBBB", RESET_PACKET, 0x00, 0x00, 0x00, RESET_PACKET))
    print "Sent reset packet to %s" % PORT_FORMAT % sys.argv[1]
    s.close()
