#!/usr/bin/env python3

#Script for drawing plots of the raw samples received from the FPGA. Read Hydrophones Code wiki entry.

import socket, struct, numpy, math
import matplotlib.pyplot as plt
from scipy import interpolate
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

RAW_PLOT_LENGTH = 64 #length of the raw plot (in samples)
HIGHEST_QUANTIZATION_LVL = RAW_PLOT_LENGTH #maximum signal amplitude (plot is square because the penguin meme is a square image)
UDP_ADDRESS = "127.0.0.1" #local host because we receive plots from hydromathd on the same machine
UDP_PAYLOAD_SIZE = 512 #size of the UDP plot packets (in bytes)
UDP_PORT = 9001 #hydromathd sends raw plots to this port

#initializing UDP networking
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_ADDRESS, UDP_PORT))

#initializing the plot window
fig = plt.figure(figsize = (7, 7))

#setting the plot labels
plt.title("Raw Data Ending in Highest Sample From Last Pinger Interval")
plt.xlabel("Sample Number")
plt.ylabel("Amplitudes ch0-Red ch1-Green ch2-Blue")

#displaying the penguin meme background
img = imread(os.path.join(os.path.dirname(os.path.realpath(__file__)), "katy.jpg"))
plt.imshow(img, extent = [-RAW_PLOT_LENGTH + 1, 0, 0, HIGHEST_QUANTIZATION_LVL - 1])

#creating the axes and setting the maximum values. removing axis ticks
ax = plt.gca()
ax.set_xlim((-RAW_PLOT_LENGTH + 1, 0)) #second quadrant because the plot captures the samples leading to the one of interest (highest one in the interval)
ax.set_ylim((0, HIGHEST_QUANTIZATION_LVL - 1))
ax.axes.yaxis.set_ticks([])

#initializing the graphs and trigger cursor with arbitrary numbers
x = numpy.arange(-RAW_PLOT_LENGTH + 1, 1)
x_smooth = numpy.linspace(-RAW_PLOT_LENGTH + 1, 0, num = "800")
y_smooth = x_smooth
(line_0, line_1, line_2) = ax.plot(x_smooth, y_smooth, 'r-', x_smooth, y_smooth, 'g-', x_smooth, y_smooth, 'b-') #'r-' for red, 'g-' for green, and 'b-' for blue

#preparing a decode strings for unpacking the received bytes arrays into values. we need 'f' for floats.
decode_string = str(RAW_PLOT_LENGTH) + 'f'

while 1:
	#initializing the graph values list
	line_values = list()

	#the three channels are received separately
	for i in range(3):
		data = bytes()

		#receiving the full packets and the final possibly partially filled packet
		for j in range(int(math.ceil(float(RAW_PLOT_LENGTH) / UDP_PAYLOAD_SIZE))):
			(data_packet, address) = sock.recvfrom(UDP_PAYLOAD_SIZE * 4)
			data += data_packet

		#unpacking the received bytes array into values
		line_values.append(numpy.asarray(struct.unpack(decode_string, data)))

	print("received raw plot")

	#updating the graphs
	function_0 = interpolate.splrep(x, line_values[0])
	smooth_line_0 = interpolate.splev(x_smooth, function_0)
	line_0.set_ydata(smooth_line_0)

	function_1 = interpolate.splrep(x, line_values[1])
	smooth_line_1 = interpolate.splev(x_smooth, function_1)
	line_1.set_ydata(smooth_line_1)

	function_2 = interpolate.splrep(x, line_values[2])
	smooth_line_2 = interpolate.splev(x_smooth, function_2)
	line_2.set_ydata(smooth_line_2)

	#not pausing after drawing a plot breaks things for some reason
	plt.draw()
	plt.pause(0.1) #in seconds


