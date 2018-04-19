import variable_filter

vf = variable_filter.VarFilter( [0], [30], [False], [True],
                                aSigma = 1e2,
                                varSigma = 1.5e0,
                                rateSigma=1e0,
                                biasSigma=1e-10)

from numpy.random import normal
import numpy as np

N = 500

var_out = np.zeros(N)
rate_out = np.zeros(N)
vbias_out = np.zeros(N)
rbias_out = np.zeros(N)

t = np.linspace(0,N/50.,N)
var_in = 30*np.sin(t) + normal(0, 1.5, (N))
rate_in = 30*np.cos(t) + normal(0,1, (N)) + 1.0

for i in range(N):
    if abs(i - 200) < 100:
        var, rate, bias = vf.update( [var_in[i]], [None]  )
    else:
        var, rate, bias = vf.update( [var_in[i]], [rate_in[i]]  )
    var_out[i] = var
    rate_out[i] = rate
    vbias_out[i] = bias[0]
    rbias_out[i] = bias[1]

import pylab
pylab.plot(t, var_out, label="var")
pylab.plot(t, var_in, label="var in")
pylab.plot(t, rate_out, label="rate")
pylab.plot(t, rate_in, label="rate in")
pylab.plot(t, vbias_out, label="vbias")
pylab.plot(t, rbias_out, label="rbias")
#pylab.plot(t, np.zeros(t.shape))
#pylab.plot(t, vbias_out, label="var bias")
#pylab.plot(t, rbias_out, label="rate bias")

pylab.legend()
pylab.show()
