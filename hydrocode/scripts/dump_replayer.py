#!/usr/bin/env python3

import socket
import sys
import time

import numpy as np

sys.path.insert(0, '../modules')
import common.const
import comms.const
import pinger.const

def getSize(file):
    file.seek(0, 2)
    size = file.tell()
    return size

# check whether dump filename was specified
try:
    dump_filename = sys.argv[1]
except IndexError:
    raise Exception('Dump filename not specified')

# load binary file specified from terminal
dump_file = open(dump_filename, 'rb')

# initialize UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
pkt_size = common.const.SAMPLE_PKT_DTYPE.itemsize

print('Replaying ' + dump_filename + '...')

# send packets
for pkt_num in range(getSize(dump_file) // pkt_size):
    # seek to the correct place in the binary file and load packet
    dump_file.seek(pkt_num * pkt_size)
    pkt_bytes = dump_file.read(pkt_size)
    pkt = np.frombuffer(pkt_bytes, dtype=common.const.SAMPLE_PKT_DTYPE)[0]

    # send packet to the correct port depending on its type
    if pkt['pkt_type'] == 0:
        sock.sendto(pkt_bytes, ('127.0.0.1', pinger.const.RECV_PORT))
    elif pkt['pkt_type'] == 1:
        sock.sendto(pkt_bytes, ('127.0.0.1', comms.const.RECV_PORT))
    else:
        raise ValueError('Valid packet types are 0 (pinger) and 1 (comms)')

    # wait for the amount of time the hydrophones board would take
    # to send another packet
    time.sleep(common.const.L_PKT / common.const.SAMPLE_RATE)