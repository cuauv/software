#!/usr/bin/env python3

#Script for replaying raw FPGA data dumps. Read Hydrophones Code wiki entry.

import socket, time, sys
import scipy.io
import numpy

PKT_LEN = 512 #total number of samples in an FPGA packet
NO_CH = 4 #number of channels
SAMPL_RATE = 200000
ADDR = "127.0.0.1" #local host because we are sending the data to the same machine
PORT = 8899 #hydromathd listens on this port

 #loading mat file specified from terminal
data = scipy.io.loadmat(sys.argv[1])

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

#sending packets
for pkt_no in range(len(data["raw_samples_interleaved"]) // PKT_LEN):

	 	#forming a packet from the data. 'H' is unsigned 16 bit integer
		send_buff = data["raw_samples_interleaved"][pkt_no * PKT_LEN : (pkt_no + 1) * PKT_LEN].astype('H')

		#converting packet into a bytes array
		payload = numpy.asarray(send_buff)
		payload.tobytes()

		#sending packet
		sock.sendto(payload, (ADDR, PORT))

		#waiting for the amount of time the FPGA would take to send another packet
		time.sleep(float(PKT_LEN) / float(NO_CH) / float(SAMPL_RATE))
