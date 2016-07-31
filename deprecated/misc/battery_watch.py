#!/usr/bin/env python
import shm


print "---------------------------"

print "V1: %.3f V" % shm.pod_starboard.cell1_v.get()
print "V2: %.3f V" % shm.pod_starboard.cell2_v.get()
print "V3: %.3f V" % shm.pod_starboard.cell3_v.get()
print "V4: %.3f V" % shm.pod_starboard.cell4_v.get()
print "V5: %.3f V" % shm.pod_starboard.cell5_v.get()
print "V6: %.3f V" % shm.pod_starboard.cell6_v.get()
print "--"


print "Capcity: %d" % shm.pod_starboard.capacity.get() 
print "Current: %.2f" % shm.pod_starboard.current.get() 
print "FG_status: %d" % shm.pod_starboard.fg_status.get() 
print "Temp: %.2f" % shm.pod_starboard.temp.get() 
print "Voltage: %.2f" % shm.pod_starboard.voltage.get() 

print "---------------------------"







