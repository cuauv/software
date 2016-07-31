#!/usr/bin/python2

# isc_bin_vocalization_demo.py
# Requires external library 'pyttsx'
# "Says" number(s) of vision-recognized bins
# Written for Ithaca Sciencenter Outreach, March 2014
# Last Updated 26 February 2014
# Questions? Email cwg46@cornell.edu

import pyttsx
import shm
from time import sleep

ttsengine = pyttsx.init()

def blockingTTS(string):
	ttsengine.say(string);
	ttsengine.runAndWait();

if __name__ == '__main__':
	while True:
		b1 = shm.shape_results_1.probability.get() > 0.5 # 10
		b2 = shm.shape_results_2.probability.get() > 0.5 # 16
		b3 = shm.shape_results_3.probability.get() > 0.5 # 37
		b4 = shm.shape_results_4.probability.get() > 0.5 # 98
		if b1: blockingTTS("ten")
		elif b2: blockingTTS("sixteen")
		elif b3: blockingTTS("thirty seven")
		elif b4: blockingTTS("ninety eight")
		sleep(1)
