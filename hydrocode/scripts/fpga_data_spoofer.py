#!/usr/bin/env python3

#Script for generating a dump containing a single simulated ping of specified frequency and location. Read Hydrophones Code wiki entry.

import math
import scipy.io

HIGHEST_QUANTIZATION_LVL = 16383 #maximum possible level of a signal
IDLE_TIME = 1.996 #padding before and after ping (in seconds)
NIPPLE_DISTANCE = 0.0178 #distance between the teats (in meters)
NO_CHANNELS = 4 #number of channels
PING_TIME = 0.004 #ping duration (in seconds)
PINGER_FREQUENCY = 30000 #ping frequency
COMMS_FREQUENCY = 52000 #comms frequency
SAMPLING_RATE = 200000
SOUND_SPEED = 1481 #speed of sound in fresh water at 20 degrees Celsius

HEADING = 45 #ping heading (in degrees)
ELEVATION = 86 #ping elevation (in degrees)
A_AMPLITUDE = 0.76 #amplitude on channel A (max 1)
B_AMPLITUDE = 0.87 #amplitude on channel B (max 1)
C_AMPLITUDE = 0.76 #amplitude on channel C (max 1)
D_AMPLITUDE = 0.75 #amplitude on channel D (max 1)

#calculating the total duration of the dump
total_time = 2 * IDLE_TIME + PING_TIME

#calculating when the signal reaches each channel. reference channel is reached exactly when the initial padding ends
a_start_time = IDLE_TIME
b_start_time = IDLE_TIME - NIPPLE_DISTANCE * math.sin(HEADING * math.pi / 180) * math.cos(ELEVATION * math.pi / 180) / SOUND_SPEED
c_start_time = IDLE_TIME + NIPPLE_DISTANCE * math.cos(HEADING * math.pi / 180) * math.cos(ELEVATION * math.pi / 180) / SOUND_SPEED
d_start_time = IDLE_TIME

#initializing the dict that will be saved to the mat file
write_dict = {"raw_samples_interleaved": []}

#generating samples
for sample_no in range(int(total_time * SAMPLING_RATE)):

	#adding DC bias, which is half the maximum possible signal level
	write_dict["raw_samples_interleaved"] += [HIGHEST_QUANTIZATION_LVL / 2, HIGHEST_QUANTIZATION_LVL / 2, HIGHEST_QUANTIZATION_LVL / 2, HIGHEST_QUANTIZATION_LVL / 2]

	#adding the sinusoidal signal to channels if the time is right. it can have at most an amplitude of half the maximum possible signal level
	if sample_no >= a_start_time * SAMPLING_RATE and sample_no < (a_start_time + PING_TIME) * SAMPLING_RATE:
		write_dict["raw_samples_interleaved"][NO_CHANNELS * sample_no + 0] += int(float(HIGHEST_QUANTIZATION_LVL) / 2 * A_AMPLITUDE * math.sin(PINGER_FREQUENCY * 2 * math.pi * (float(sample_no) / SAMPLING_RATE - a_start_time)))

	if sample_no >= b_start_time * SAMPLING_RATE and sample_no < (b_start_time + PING_TIME) * SAMPLING_RATE:
		write_dict["raw_samples_interleaved"][NO_CHANNELS * sample_no + 1] += int(float(HIGHEST_QUANTIZATION_LVL) / 2 * B_AMPLITUDE * math.sin(PINGER_FREQUENCY * 2 * math.pi * (float(sample_no) / SAMPLING_RATE - b_start_time)))

	if sample_no >= c_start_time * SAMPLING_RATE and sample_no < (c_start_time + PING_TIME) * SAMPLING_RATE:
		write_dict["raw_samples_interleaved"][NO_CHANNELS * sample_no + 2] += int(float(HIGHEST_QUANTIZATION_LVL) / 2 * C_AMPLITUDE * math.sin(PINGER_FREQUENCY * 2 * math.pi * (float(sample_no) / SAMPLING_RATE - c_start_time)))

	if sample_no >= d_start_time * SAMPLING_RATE and sample_no < (d_start_time + PING_TIME) * SAMPLING_RATE:
		write_dict["raw_samples_interleaved"][NO_CHANNELS * sample_no + 3] += int(float(HIGHEST_QUANTIZATION_LVL) / 2 * D_AMPLITUDE * math.sin(COMMS_FREQUENCY * 2 * math.pi * (float(sample_no) / SAMPLING_RATE - d_start_time)))

#saving to the mat file
scipy.io.savemat("spoofed_dump.mat", write_dict, do_compression = True, oned_as = "column")

