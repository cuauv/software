#!/usr/bin/env python3

#Script for replaying raw FPGA data dumps. Read Hydrophones Code wiki entry.

import socket, time, sys
import scipy.io
import numpy

TOTAL_PACKET_LENGTH = 512 #total number of samples in an FPGA packet
NO_CHANNELS = 4 #number of channels
SAMPLING_RATE = 200000
UDP_ADDRESS = "127.0.0.1" #local host because we are sending the data to the same machine
UDP_PORT = 8899 #hydromathd listens on this port

 #loading mat file specified from terminal
data = scipy.io.loadmat(sys.argv[1])

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

#sending packets
for packet_no in range(len(data["raw_samples_interleaved"]) // TOTAL_PACKET_LENGTH):

	 	#forming a packet from the data. 'H' is unsigned 16 bit integer
		send_buffer = data["raw_samples_interleaved"][packet_no * TOTAL_PACKET_LENGTH : (packet_no + 1) * TOTAL_PACKET_LENGTH].astype('H')

		#converting packet into a bytes array
		payload = numpy.asarray(send_buffer)
		payload.tobytes()

		#sending packet
		sock.sendto(payload, (UDP_ADDRESS, UDP_PORT))

		#waiting for the amount of time the FPGA would take to send another packet
		time.sleep(float(TOTAL_PACKET_LENGTH / NO_CHANNELS) / float(SAMPLING_RATE))
