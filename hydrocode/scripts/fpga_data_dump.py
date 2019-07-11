#!/usr/bin/env python3

#Script for dumping raw FPGA data to a mat file. Read Hydrophones Code wiki entry.

import socket, struct, time
import scipy.io

TOTAL_PACKET_LENGTH = 512 #total number of samples in an FPGA packet
UDP_ADDRESS = "" #can receive from any address
UDP_PORT = 8899 #FPGA sends packets to this port

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_ADDRESS, UDP_PORT))

#preparing a decode string for unpacking the received bytes array into values. we need 'H' for unsigned 16 bit integers
decode_string = str(TOTAL_PACKET_LENGTH) + 'H'

#initializing the dict that will be saved to the mat file
write_dict = {"raw_samples_interleaved": []}

packet_no = 0
start_time = time.time()
last_print_time = start_time

try:
	while 1:
		#receiving a packet using a blocking socket
		(data, address) = sock.recvfrom(TOTAL_PACKET_LENGTH * 2)

		#unpacking the received bytes array into values
		write_dict["raw_samples_interleaved"] += struct.unpack(decode_string, data)

		packet_no += 1

		#printing packets per second
		if packet_no % 3125 == 0:
			print('\n' + str(int(3125 / (time.time() - last_print_time))) + " packets per second")
			last_print_time = time.time()

#ending the dump upon keyboard interrupt
except KeyboardInterrupt:
	print("\ndumped " + str(packet_no) + " packets in " + "{:.2f} seconds".format(time.time() - start_time))
	print("writing to file")

	#saving to the mat file
	scipy.io.savemat("dump.mat", write_dict, do_compression = True, oned_as = "column")
