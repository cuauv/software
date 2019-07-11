#!/usr/bin/env python3

#Script for drawing plots of the correct DFT bin amplitude and ratio between successive amplitudes with respect to time. Also shows trigger point. Read Hydrophones Code wiki entry.

import socket, struct, numpy, math
import matplotlib.pyplot as plt
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

DFT_PLOT_LENGTH = 3500 #length of the dft plot (in samples)
MAXIMUM_AMPLITUDE = DFT_PLOT_LENGTH #maximum signal amplitude (plot is square because the penguin meme is a square image)
UDP_ADDRESS = "127.0.0.1" #local host because we receive plots from hydromathd on the same machine
UDP_PAYLOAD_SIZE = 512 #size of the UDP plot packets (in bytes)
UDP_PORT = 9003 #hydromathd sends dft plots to this port

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_ADDRESS, UDP_PORT))

#initializing the plot window
fig = plt.figure(figsize = (7, 7))

#setting the plot labels
plt.title("Correct DFT bin Amplitudes and Amplitude Ratios w/ Trigger Point")
plt.xlabel("Packet Number")
plt.ylabel("Amplitudes White, Ratios Blue")

#displaying the penguin meme background
img = imread(os.path.join(os.path.dirname(os.path.realpath(__file__)), "katy.jpg"))
plt.imshow(img, extent = [0, DFT_PLOT_LENGTH - 1, 0, MAXIMUM_AMPLITUDE - 1])

#creating the axes and setting the maximum values. removing axis ticks
ax = plt.gca()
ax.set_xlim((0, DFT_PLOT_LENGTH - 1))
ax.set_ylim((0, MAXIMUM_AMPLITUDE - 1))
ax.axes.yaxis.set_ticks([])

#initializing the graphs and trigger cursor with arbitrary numbers
x = numpy.arange(0, DFT_PLOT_LENGTH)
y = x
trigger_cursor = plt.axvline(x = 0, color = "yellow", ymin = 0.95)
(line_0, line_1) = ax.plot(x, y, 'w-', x, y, 'b-', linewidth = '0.2', marker = '.', markersize = '2') #'w-' for white and 'b-' for blue

#preparing a decode strings for unpacking the received bytes arrays into values. we need 'f' for floats. the trigger point array contains a single value
decode_string = str(DFT_PLOT_LENGTH) + 'f'
trigger_decode_string = str(1) + 'f'

while 1:
	#initializing the graph values list
	line_values = list()

	#amplitudes are received first, then ratios
	for i in range(2):
		data = bytes()

		#receiving the full packets and the final possibly partially filled packet
		for j in range(int(math.ceil(float(DFT_PLOT_LENGTH) / UDP_PAYLOAD_SIZE))):
			(data_packet, address) = sock.recvfrom(UDP_PAYLOAD_SIZE * 4)
			data += data_packet

		#unpacking the received bytes array into values
		line_values.append(numpy.asarray(struct.unpack(decode_string, data)))

	#receiving the trigger point number
	(data_packet, address) = sock.recvfrom(1 * 4)

	#unpacking the trigger point number
	trigger_point = struct.unpack(trigger_decode_string, data_packet)[0]

	print("received dft amplitude plot")

	#updating the graphs
	line_0.set_ydata(line_values[0]) #amplitudes
	line_1.set_ydata(line_values[1]) #ratios
	trigger_cursor.set_xdata(trigger_point) #trigger packet number

	#not pausing after drawing a plot breaks things for some reason
	plt.draw()
	plt.pause(0.1) #in seconds


