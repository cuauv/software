#!/usr/bin/env python3

import socket, struct, numpy, math
import matplotlib.pyplot as plt
from scipy import interpolate
import os.path
try:
    from cv2 import imread
except ImportError:
    from scipy.misc import imread

LEN = 10 * 10**3
ADDR = "127.0.0.1"
PKT_SIZE = 512
PORT = 9004

BIT_DEPTH = 16383

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((ADDR, PORT))

fig = plt.figure(figsize = (7, 7))

plt.subplot(2, 1, 1)
plt.title("Comms Filtered Plot Amplitude")

ax = plt.gca()
ax.set_xlim(-LEN + 1, 0)
ax.set_ylim(0, BIT_DEPTH / 2)

x = numpy.arange(-LEN + 1, 1)
x_smooth = numpy.linspace(-LEN + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-LEN + 1, 1, LEN // 10))

(ampl_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

plt.subplot(2, 1, 2)
plt.title("Comms Filtered Plot Phase")

ax = plt.gca()
ax.set_xlim(-LEN + 1, 0)
ax.set_ylim(-2 * math.pi, 2 * math.pi)

x = numpy.arange(-LEN + 1, 1)
x_smooth = numpy.linspace(-LEN + 1, 0, num = "10000")
y_smooth = x_smooth
plt.xticks(numpy.arange(-LEN + 1, 1, LEN // 10))

(ph_line, ) = ax.plot(x_smooth, y_smooth, 'k-', linewidth = 0.5)

decode_str = str(2 * LEN) + 'f'

while 1:
	functions = list()
	smooth_values = list()

	data = bytes()

	for i in range(int(math.ceil(float(2 * LEN) * 4.0 / PKT_SIZE))):
		(data_pkt, recv_addr) = sock.recvfrom(PKT_SIZE)
		data += data_pkt

	interleaved_values = numpy.asarray(struct.unpack(decode_str, data))
	real_values = interleaved_values[0::2]
	imag_values = interleaved_values[1::2]

	print("received comms filtered plot")

	real_function = interpolate.splrep(x, real_values)
	smooth_real_values = interpolate.splev(x_smooth, real_function)

	imag_function = interpolate.splrep(x, imag_values)
	smooth_imag_values = interpolate.splev(x_smooth, imag_function)

	ampl_line.set_ydata(numpy.abs(smooth_real_values + 1j * smooth_imag_values))
	ph_line.set_ydata(numpy.angle(smooth_real_values + 1j * smooth_imag_values))

	plt.draw()
	plt.pause(0.1)


