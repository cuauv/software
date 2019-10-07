#!/usr/bin/env python3

import socket, struct, numpy, math
import matplotlib.pyplot as plt
from scipy import interpolate
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

LEN = 1000;
ADDR = "127.0.0.1"
PKT_SIZE = 512
PORT = 9005

BIT_DEPTH = 16383

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((ADDR, PORT))

fig = plt.figure(figsize = (7, 7))

plt.subplot(2, 1, 1)
plt.title("Correlation Input Plot")

ax = plt.gca()
ax.set_xlim(-LEN + 1, 0)
ax.set_ylim(-BIT_DEPTH / 2, BIT_DEPTH / 2)

x = numpy.arange(-LEN + 1, 1)
x_smooth = numpy.linspace(-LEN + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-LEN + 1, 1, LEN // 10))

(in_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

plt.subplot(2, 1, 2)
plt.title("Correlation Output Plot")

ax = plt.gca()
ax.set_xlim(-LEN + 1, 0)

x = numpy.arange(-LEN + 1, 1)
x_smooth = numpy.linspace(-LEN + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-LEN + 1, 1, LEN // 10))

(sig_out_line, noise_out_line, thresh_line) = ax.plot(x_smooth, y_smooth, 'g-', x_smooth, y_smooth, 'r-', x_smooth, y_smooth, 'b-', linewidth = 0.5)

decode_str = str(LEN) + 'f'

while 1:
	line_values = list()
	functions = list()
	smooth_values = list()

	for i in range(4):
		data = bytes()

		for i in range(int(math.ceil(float(LEN) * 4.0 / PKT_SIZE))):
			(data_pkt, recv_addr) = sock.recvfrom(PKT_SIZE)
			data += data_pkt

		line_values.append(numpy.asarray(struct.unpack(decode_str, data)))

	print("received corr plot")

	for i in range(4):
		functions.append(interpolate.splrep(x, line_values[i]))
		smooth_values.append(interpolate.splev(x_smooth, functions[i]))

	in_line.set_ydata(smooth_values[0])
	sig_out_line.set_ydata(smooth_values[1])
	noise_out_line.set_ydata(smooth_values[2])
	thresh_line.set_ydata(smooth_values[3])

	ax.set_ylim(min([smooth_values[1].min(), smooth_values[2].min(), smooth_values[3].min()]), max([smooth_values[1].max(), smooth_values[2].max(), smooth_values[3].max()]))

	plt.draw()
	plt.pause(0.1)


