#!/usr/bin/env python2
import shm
from time import sleep

SAMPLES = 500 #Samples to obtain from pitch and roll
SAMPLE_INTERVAL = .01 #Seconds between each sample

pitch_roll = {'pitch': shm.threedmg.pitch,
              'roll': shm.threedmg.roll
              }
pr_vals = pitch_roll.values()
x = dict((z,0) for z in pitch_roll.keys())

shm.threedmg_settings.pitch_offset.set(0)
shm.threedmg_settings.roll_offset.set(0)

print "Resetting pitch and roll... do not touch sub!"

for i in range(SAMPLES):
    for k,v in pitch_roll.items():
        x[k] += v.get()
    sleep(SAMPLE_INTERVAL)

for j in pitch_roll.keys():
    x[j] = x[j]/SAMPLES

shm.threedmg_settings.pitch_offset.set(x['pitch'])
shm.threedmg_settings.roll_offset.set(x['roll'])
print('Recalibrated pitch and roll')
