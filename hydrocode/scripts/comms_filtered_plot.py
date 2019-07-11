#!/usr/bin/env python3

import socket, struct, numpy, math
import matplotlib.pyplot as plt
from scipy import interpolate
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

COMMS_FILTERED_PLOT_DURATION = 10**6
SAMPLE_RATE = 10 * 10**3
UDP_ADDRESS = "127.0.0.1"
UDP_PAYLOAD_SIZE = 512
UDP_PORT = 9004

COMMS_FILTERED_PLOT_LENGTH = COMMS_FILTERED_PLOT_DURATION * SAMPLE_RATE // 10**6
HIGHEST_QUANTIZATION_LVL = 16383

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_ADDRESS, UDP_PORT))

amplitude_fig = plt.figure(figsize = (7, 7))

plt.subplot(2, 1, 1)
plt.title("Comms Filtered Plot Amplitude")

ax = plt.gca()
ax.set_xlim(-COMMS_FILTERED_PLOT_LENGTH + 1, 0)
ax.set_ylim(0, HIGHEST_QUANTIZATION_LVL / 2)

x = numpy.arange(-COMMS_FILTERED_PLOT_LENGTH + 1, 1)
x_smooth = numpy.linspace(-COMMS_FILTERED_PLOT_LENGTH + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-COMMS_FILTERED_PLOT_LENGTH + 1, 1, COMMS_FILTERED_PLOT_LENGTH // 10))

(amplitudes_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

plt.subplot(2, 1, 2)
plt.title("Comms Filtered Plot Phase")

ax = plt.gca()
ax.set_xlim(-COMMS_FILTERED_PLOT_LENGTH + 1, 0)
ax.set_ylim(-2 * math.pi, 2 * math.pi)

x = numpy.arange(-COMMS_FILTERED_PLOT_LENGTH + 1, 1)
x_smooth = numpy.linspace(-COMMS_FILTERED_PLOT_LENGTH + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-COMMS_FILTERED_PLOT_LENGTH + 1, 1, COMMS_FILTERED_PLOT_LENGTH // 10))

(phases_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

decode_string = str(COMMS_FILTERED_PLOT_LENGTH) + 'f'

while 1:
	line_values = list()
	functions = list()
	smooth_values = list()

	for i in range(2):
		data = bytes()

		for i in range(int(math.ceil(float(COMMS_FILTERED_PLOT_LENGTH) / UDP_PAYLOAD_SIZE))):
			(data_packet, address) = sock.recvfrom(UDP_PAYLOAD_SIZE * 4)
			data += data_packet

		line_values.append(numpy.asarray(struct.unpack(decode_string, data)))

	print("received comms filtered plot")

	for i in range(2):
		functions.append(interpolate.splrep(x, line_values[i]))
		smooth_values.append(interpolate.splev(x_smooth, functions[i]))

	amplitudes_line.set_ydata(numpy.abs(smooth_values[0] + 1j * smooth_values[1]))
	phases_line.set_ydata(numpy.angle(smooth_values[0] + 1j * smooth_values[1]))

	plt.draw()
	plt.pause(0.1)


