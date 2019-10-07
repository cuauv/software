#!/usr/bin/env python3

#Script for generating a dump containing a single simulated ping of specified frequency and location. Read Hydrophones Code wiki entry.

import math
import scipy.io

BIT_DEPTH = 16383 #maximum possible level of a signal
IDLE_TIME = 1.996 #padding before and after ping (in seconds)
NIPPLE_DIST = 0.0178 #distance between the teats (in meters)
NO_CH = 4 #number of channels
PING_TIME = 0.004 #ping duration (in seconds)
FREQ = 30000 #ping frequency
COMMS_FREQ = 52000 #comms frequency
SAMPL_RATE = 200000
SOUND_SPEED = 1481 #speed of sound in fresh water at 20 degrees Celsius

HDG = 45 #ping heading (in degrees)
ELEV = 86 #ping elevation (in degrees)
A_AMPL = 0.76 #amplitude on channel A (max 1)
B_AMPL = 0.87 #amplitude on channel B (max 1)
C_AMPL = 0.76 #amplitude on channel C (max 1)
D_AMPL = 0.75 #amplitude on channel D (max 1)

#calculating the total duration of the dump
total_time = 2 * IDLE_TIME + PING_TIME

#calculating when the signal reaches each channel. reference channel is reached exactly when the initial padding ends
a_start_time = IDLE_TIME
b_start_time = IDLE_TIME - NIPPLE_DIST * math.sin(HDG * math.pi / 180) * math.cos(ELEV * math.pi / 180) / SOUND_SPEED
c_start_time = IDLE_TIME + NIPPLE_DIST * math.cos(HDG * math.pi / 180) * math.cos(ELEV * math.pi / 180) / SOUND_SPEED
d_start_time = IDLE_TIME

#initializing the dict that will be saved to the mat file
write_dict = {"raw_samples_interleaved": []}

#generating samples
for sampl_no in range(int(total_time * SAMPL_RATE)):

	#adding DC bias, which is half the maximum possible signal level
	write_dict["raw_samples_interleaved"] += [BIT_DEPTH / 2, BIT_DEPTH / 2, BIT_DEPTH / 2, BIT_DEPTH / 2]

	#adding the sinusoidal signal to channels if the time is right. it can have at most an amplitude of half the maximum possible signal level
	if sampl_no >= a_start_time * SAMPL_RATE and sampl_no < (a_start_time + PING_TIME) * SAMPL_RATE:
		write_dict["raw_samples_interleaved"][NO_CH * sampl_no + 0] += int(float(BIT_DEPTH) / 2 * A_AMPL * math.sin(FREQ * 2 * math.pi * (float(sampl_no) / SAMPL_RATE - a_start_time)))

	if sampl_no >= b_start_time * SAMPL_RATE and sampl_no < (b_start_time + PING_TIME) * SAMPL_RATE:
		write_dict["raw_samples_interleaved"][NO_CH * sampl_no + 1] += int(float(BIT_DEPTH) / 2 * B_AMPL * math.sin(FREQ * 2 * math.pi * (float(sampl_no) / SAMPL_RATE - b_start_time)))

	if sampl_no >= c_start_time * SAMPL_RATE and sampl_no < (c_start_time + PING_TIME) * SAMPL_RATE:
		write_dict["raw_samples_interleaved"][NO_CH * sampl_no + 2] += int(float(BIT_DEPTH) / 2 * C_AMPL * math.sin(FREQ * 2 * math.pi * (float(sampl_no) / SAMPL_RATE - c_start_time)))

	if sampl_no >= d_start_time * SAMPL_RATE and sampl_no < (d_start_time + PING_TIME) * SAMPL_RATE:
		write_dict["raw_samples_interleaved"][NO_CH * sampl_no + 3] += int(float(BIT_DEPTH) / 2 * D_AMPL * math.sin(FREQ * 2 * math.pi * (float(sampl_no) / SAMPL_RATE - d_start_time)))

#saving to the mat file
scipy.io.savemat("spoofed_dump.mat", write_dict, do_compression = True, oned_as = "column")

