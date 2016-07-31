#!/usr/bin/env python

import shm

# isc_marker_demo.py
# Launches markers (in theory)
# Written for Ithaca Sciencenter Outreach, March 2014
# Last Updated 1 March 2014
# Questions? Email cwg46@cornell.edu

if __name__ == '__main__':
    shm.actuator_2.trigger.set(True)
