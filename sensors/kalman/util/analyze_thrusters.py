'''
See measure_thrusters.py for details.
Runs analysis on output from that file.
'''

import numpy
import pylab
from measure_thrusters import thrusters, MEASUREMENT_PERIOD, pwm_values
from smooth import smooth


PRE_SMOOTHING = 10 # Width of smoothing to apply to output
SMOOTHING = 5

pylab.ion()

# Write output
import pickle
data = pickle.load(open("thrusts.pickle","r"))

# Perform analysis
gs = dict((t,dict()) for t in thrusters)
Gs = dict()
for t in thrusters:
    pylab.clf()
    for v in pwm_values:
        measurements = data[t][v]
        measurements = smooth(measurements, PRE_SMOOTHING, 'flat')[PRE_SMOOTHING-1:-1-PRE_SMOOTHING]
        diff = measurements[1:] - measurements[:-1]

        # TODO: should this use v or pwm_to_thrust(v)?
        # I would expect the system to be linear in thrust, not PWM
        # but using the PWM value directly gives more consistent results
        #g = diff / pwm_to_thrust(v,thrusters_convert[t]) / MEASUREMENT_PERIOD
        g = diff / v

        gs[t][v] = smooth(g,SMOOTHING,'flat')[SMOOTHING-1:-1-SMOOTHING]
        pylab.plot(gs[t][v], label='%s,%s'%(t,v))
        #pylab.plot(measurements, label='m %s,%s'%(t,v))
    # Compute the average for each thruster
    G = numpy.zeros( max(len(s) for s in gs[t].values()) )
    for s in gs[t].values():
        G += s
    G /= len(gs[t])
    Gs[t] = G
    pylab.plot(G, linewidth=5)

    pylab.legend()
    #pylab.show()
    raw_input("[ENTER] to continue")
pickle.dump(gs, open("impulses.pickle","w"))
pickle.dump(Gs, open("impulse_responses.pickle","w"))
