#!/usr/bin/env auv-python2
''' Utility to set current heading to be zero after linearization '''

#from auval.shmem import SharedVar
import shm
import LUTlinearizer

#hdg = SharedVar("/kalman/heading").get()
hdg = shm.kalman.heading.get()

linearized = file("linear_heading_LUT").read().split("\n")
linearized = [float(x) for x in linearized if len(x) > 0]

out = [(l-hdg)%360. for l in linearized]

out_file = file('linear_heading_LUT', 'w')
out_file.write( "\n".join([str(h) for h in out]) )

print "Changed heading by angle %s" % (-hdg)

