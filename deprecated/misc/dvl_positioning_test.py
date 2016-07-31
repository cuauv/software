#!/usr/bin/env python2
"incredibly simple means of monitoring the various DVL outputs and related values"
from auval.vehicle import *
from time import sleep
import matplotlib.pyplot as plt

def format(f):
    #Turns number into a short string
    #There has got to be a better way to do this
    s = str(f)
    s = s[:s.find(".")+3]
    s = " "*(6-len(s)) + s
    return s

#Comment out or in the variables you want to remove or see
variables = [
            #("kn","dmg_north"),
            #("ke","dmg_east"),
            #("kf","dmg_forward"),
            #("ks","dmg_sway"),
            #("dx","dmg_x"),
            #("dy","dmg_y"),
            #("dn","dvl_dmg_east"),
            #("de","dvl_dmg_north"),
            #("dvx","dvl_velx"),
            #("dvy","dvl_vely"),
            ("hdg","heading"),
            #("tcm", "tcm5"),
            ("ocs", "oceanserver"),
            ("dvlh", "dvl_heading"),
            ("avgh", "heading_avg")
            ]

log = []
try:
    while(True):
        values = [(nick, shared_vars[var].get()) for nick,var in variables]
        for nick, value in values:
            print "{0}:{1}".format(nick, format(value)),
        print ""
        log.append([value for nick, value in values])
        sleep(0.5)
except KeyboardInterrupt:
    pass


#matplotlib Plotting stuffs
plot_formats = ["b-", "r-", "g-", "y-", "bs-", "rs-", "gs-", "ys-"]

for i in range(4):#len(variables)): 
     y_values = [entry[i] for entry in log]
     plt.plot(y_values,plot_formats[i%len(plot_formats)], label=variables[i][1])

plt.legend(loc="upper left")
plt.show()

