#!/usr/bin/env python
import socket, time, struct, sys
import scipy.io

import collections
import numpy, scipy
CHANNEL_DEPTH = 128
UDP_PAYLOAD_SIZE = 818 #Derived from wireshark.
UDP_IP="127.0.0.1" #Send to the same computer
UDP_PORT=8899

if len(sys.argv) != 2:
    print("Usage: udp_send_matr.py dump_file.mat")
    sys.exit(1)

# Load ping data from a previous recording by udp_recv_matr.py
d = scipy.io.loadmat(sys.argv[1])
print("Playing back hydrophones recording. Data file information:")
print(d['__header__'])
dat_A = d['ping_A'][0]
dat_B = d['ping_B'][0]
dat_C = d['ping_C'][0]

# Verify the data length is sane (an exact multiple of packet length)
dat_len = len(dat_A)
assert dat_len % CHANNEL_DEPTH == 0

# Allocate a buffer for the data we're sending
send_dat = numpy.zeros((int(UDP_PAYLOAD_SIZE/2),), dtype=numpy.int16)
# Create the socket for sending
# NOTE: since sendto() is used, no bind needs to occur here
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

# Track total sent
num_sent = 0
start_time = time.perf_counter()

# Main packet sending loop
for idx in range(int(dat_len / CHANNEL_DEPTH)):
    # Compute range in the data arrays contained by this packet
    start_idx = idx*CHANNEL_DEPTH
    end_idx   = (idx+1)*CHANNEL_DEPTH

    # Write the data into the transmit buffer. Note that all 3 channels are interleaved
    # There is also padding at the end of the buffer which is not written to, and is ignored
    send_dat[0:CHANNEL_DEPTH*3:3] = dat_A[start_idx:end_idx]
    send_dat[1:CHANNEL_DEPTH*3:3] = dat_B[start_idx:end_idx]
    send_dat[2:CHANNEL_DEPTH*3:3] = dat_C[start_idx:end_idx]

    # Convert data to bytes and send
    dat = send_dat.tobytes()
    sock.sendto(dat, (UDP_IP, UDP_PORT))

    num_sent += 1

    # The receive program will drop packets if we send them too fast. To avoid this,
    # send packets at roughly the same rate as the real board by sleeping
    time.sleep(0.00032) # Amount of time to gather 128 samples at a 400kHz sample rate

# Report total time elapsed
end_time = time.perf_counter()
elapsed_time = end_time - start_time
print("Sent {} packets in {} seconds".format(num_sent, elapsed_time))
