#!/usr/bin/env python2
'''
Script to set up and run local simulation using simulator
'''
import shm
import os

os.chdir(os.environ['CUAUV_SOFTWARE'])
CONTROLLER = "auv-controld3"

print "Starting Thruster Daemon..."
os.system("%s &" % CONTROLLER)

#print "Starting VSP viewer forward & downward..."
#os.system("auv-vsp-viewer forward &")
#os.system("auv-vsp-viewer downward &")

# TODO vision in sim is broken :(
#print "Starting simulated vision..."
#os.system("auv-visiond -g $CUAUV_SOFTWARE/vision/tests/sim_test &")

print "Adjusting Shared Memory..."
shm.settings_control.enabled.set(True)
shm.switches.soft_kill.set(False)
shm.settings_control.depth_active.set(1)
shm.settings_control.pitch_active.set(1)
shm.settings_control.heading_active.set(1)
shm.settings_control.velx_active.set(1)
shm.settings_control.vely_active.set(1)
shm.settings_control.drag_forces.set(0)

shm.settings_control.quat_pid.set(1)
shm.settings_quat.kP.set(2.0)
shm.settings_quat.kI.set(0.0)
shm.settings_quat.kD.set(0.0)

print "Starting Simulator..."
os.chdir('simulator')
os.system("python sim2.py")

#At this point, the simulator has been closed and we can clean up.

print "Simulator has been closed... cleaning up"
os.system("pkill -f %s" % CONTROLLER)
