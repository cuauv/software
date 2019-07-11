#!/usr/bin/env python3

import socket, struct, numpy, math
import matplotlib.pyplot as plt
from scipy import interpolate
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

CORR_PLOT_DURATION = 5 * 10**6
SAMPLE_RATE = 200
UDP_ADDRESS = "127.0.0.1"
UDP_PAYLOAD_SIZE = 512
UDP_PORT = 9005

CORR_PLOT_LENGTH = CORR_PLOT_DURATION * SAMPLE_RATE // 10**6
HIGHEST_QUANTIZATION_LVL = 16383

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_ADDRESS, UDP_PORT))

amplitude_fig = plt.figure(figsize = (7, 7))

plt.subplot(2, 1, 1)
plt.title("Correlation Input Plot")

ax = plt.gca()
ax.set_xlim(-CORR_PLOT_LENGTH + 1, 0)
ax.set_ylim(-HIGHEST_QUANTIZATION_LVL / 2, HIGHEST_QUANTIZATION_LVL / 2)

x = numpy.arange(-CORR_PLOT_LENGTH + 1, 1)
x_smooth = numpy.linspace(-CORR_PLOT_LENGTH + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-CORR_PLOT_LENGTH + 1, 1, CORR_PLOT_LENGTH // 10))

(input_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

plt.subplot(2, 1, 2)
plt.title("Correlation Output Plot")

ax = plt.gca()
ax.set_xlim(-CORR_PLOT_LENGTH + 1, 0)

x = numpy.arange(-CORR_PLOT_LENGTH + 1, 1)
x_smooth = numpy.linspace(-CORR_PLOT_LENGTH + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-CORR_PLOT_LENGTH + 1, 1, CORR_PLOT_LENGTH // 10))

(signal_output_line, noise_output_line, thresh_line) = ax.plot(x_smooth, y_smooth, 'g-', x_smooth, y_smooth, 'r-', x_smooth, y_smooth, 'b-', linewidth = 0.5)

decode_string = str(CORR_PLOT_LENGTH) + 'f'

while 1:
	line_values = list()
	functions = list()
	smooth_values = list()

	for i in range(4):
		data = bytes()

		for i in range(int(math.ceil(float(CORR_PLOT_LENGTH) / UDP_PAYLOAD_SIZE))):
			(data_packet, address) = sock.recvfrom(UDP_PAYLOAD_SIZE * 4)
			data += data_packet

		line_values.append(numpy.asarray(struct.unpack(decode_string, data)))

	print("received corr plot")

	for i in range(4):
		functions.append(interpolate.splrep(x, line_values[i]))
		smooth_values.append(interpolate.splev(x_smooth, functions[i]))

	input_line.set_ydata(smooth_values[0])
	signal_output_line.set_ydata(smooth_values[1])
	noise_output_line.set_ydata(smooth_values[2])
	thresh_line.set_ydata(smooth_values[3])

	ax.set_ylim(min([smooth_values[1].min(), smooth_values[2].min(), smooth_values[3].min()]), max([smooth_values[1].max(), smooth_values[2].max(), smooth_values[3].max()]))

	plt.draw()
	plt.pause(0.1)


