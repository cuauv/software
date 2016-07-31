#!/usr/bin/env python

# isc_torpedo_demo.py
# Fires torpedo when target is adequately centered
# Written for Ithaca Sciencenter Outreach, March 2014
# Last Updated 1 March 2014
# Questions? Email cwg46@cornell.edu

import shm

MIN_AREA = 15000
MIN_PROB = 0.5
MIN_SKEW = 1.347
LEFT = False
RIGHT = True

if __name__ == '__main__':
    while True:
        if shm.nest_results.area.get() > MIN_AREA:
            if shm.nest_results.probability.get() > MIN_PROB:
                if shm.nest_results.skew.get() > MIN_SKEW:
                    if shm.nest_results.targeting.get() >= 2:
                        if LEFT: shm.actuator_0.trigger.set(True) # Left
                        elif RIGHT: shm.actuator_1.trigger.set(True) # Right
                    else: print('Targeting value too low.')
                else: print('Skew too high')
            else: print('Probability too small.')
        else: print('Area too small; move the target closer.')
