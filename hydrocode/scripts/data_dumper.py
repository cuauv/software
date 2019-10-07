#!/usr/bin/env python3

#Script for dumping raw FPGA data to a mat file. Read Hydrophones Code wiki entry.

import socket, struct, time
import scipy.io

PKT_LEN = 512 #total number of samples in an FPGA packet
ADDR = "" #can receive from any address
PORT = 8899 #FPGA sends packets to this port

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((ADDR, PORT))

#preparing a decode string for unpacking the received bytes array into values. we need 'H' for unsigned 16 bit integers
decode_str = str(PKT_LEN) + 'H'

#initializing the dict that will be saved to the mat file
write_dict = {"raw_samples_interleaved": []}

pkt_no = 0
start_time = time.time()
last_print_time = start_time

try:
	while 1:
		#receiving a packet using a blocking socket
		(data, recv_addr) = sock.recvfrom(PKT_LEN * 2)

		#unpacking the received bytes array into values
		write_dict["raw_samples_interleaved"] += struct.unpack(decode_str, data)

		pkt_no += 1

		#printing packets per second
		if pkt_no % 3125 == 0:
			print('\n' + str(int(3125 / (time.time() - last_print_time))) + " packets per second")
			last_print_time = time.time()

#ending the dump upon keyboard interrupt
except KeyboardInterrupt:
	print("\ndumped " + str(pkt_no) + " packets in " + "{:.2f} seconds".format(time.time() - start_time))
	print("writing to file")

	#saving to the mat file
	scipy.io.savemat("dump.mat", write_dict, do_compression = True, oned_as = "column")
