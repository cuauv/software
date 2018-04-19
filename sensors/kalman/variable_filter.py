import generic_kalman
import settings

import numpy as np
from numpy import array

class VarFilter(generic_kalman.KalmanFilter):
    ''' This is a very generic Kalman filter for the case where we have a single variable
        and we read both its value and its rate, but not its second derivative.
        (Eg: an angle and angular rate, or a velocity and acceleration)
        Importantly, all of these measurements may have 'biases' to them, so that, eg,
        we might be measuring var+biases rather than just var.
    '''

    def __init__(self,
                   var_sensors,
                   rate_sensors,
                   var_biases,
                   rate_biases,

                   bias_guess = None,

                   #Settings:
                   varSigma = 15e-1,
                   rateSigma = 1,
                   aSigma = 1e5,
                   biasSigma = 1e-8,
                   vel_drag = 1.0,
                   mod360 = False):
        '''
            var_sensors, rate_sensors are lists of values giving starting values
            var_biases, rate_biases are lists of True/False for whether these have a bias
            (Usually all will be True, except in cases like angles you want one compass to be defined
             to give 'north' when it reads 0 - ie. no bias.)
        '''
            

        # Whether we're an anglular variable
        self.mod360 = mod360

        assert(len(var_sensors) == len(var_biases))
        assert(len(rate_sensors) == len(rate_biases))

        n_var = len(var_sensors)
        n_rate = len(rate_sensors)
        self.n_var = n_var
        self.n_rate = n_rate

        if bias_guess is None:
            bias_guess = np.zeros( n_var + n_rate)

        #State size
        #form: var, rate, var biases, rate biases
        n = 2 + n_var + n_rate

        # Input size
        l = n

        #Measurement size
        #form: vars, rates
        m = n_var + n_rate

        #Settings
        dt = settings.dt #Simulation timestep

        #A n-by-n relates one state to the next
        # The rate and the biases don't change from one step to the next
        A = np.identity(n)
        # but we do add rate to var every
        A[0,1] = dt
        A[1,1] = vel_drag # drag constant for velocity
        #print "A:", A

        #B n-by-l relates control input to the state
        B = np.identity(n)

        #H m-by-n relates the state to the measurement
        H = np.zeros( (m,n) )
        H[:n_var, 0] = 1 # var sensors measure var directly
        H[n_var:n_var+n_rate, 1] = 1 # rate sensors measure rate directly
        # Now add on the biases
        H[:,2:] = np.diag(var_biases + rate_biases) # Rate sensors measure the rate

        #Q n-by-n process noise covariance
        Q = np.identity(n)
        Q[:2,:2] =  aSigma**2*array([[dt**4/4, dt**3/2],
                                     [dt**3/2, dt**2]])
        (Q[2:,2:])[np.diag_indices_from(Q[2:,2:])] *= biasSigma**2
        #print "Q: \n", Q

        #R m-by-m measurement noise covariance
        R = np.identity(m)
        R[:n_var, :n_var] *= varSigma**2
        R[n_var:, n_var:] *= rateSigma**2
        #print "R: \n", R

        #P n-by-n starting estimate uncertainties
        # This is chosen pretty arbitrarily
        # but it should converge regardless of this
        P = np.identity(n)
        #P[0,0] = varSigma**2
        #P[1,1] *= rateSigma**2
        #P[2:,2:] *= biasSigma**2
        P[2:,2:] *= 1e1
        #P += 1e-4 # Eliminate zeros - everything could maybe be correlated?
        #print "P:\n", P

        xHatStart = np.concatenate( [[var_sensors[0]], [rate_sensors[0]], bias_guess] )
        super(VarFilter, self).__init__( n,m,l, xHatStart,P, A,B,H,R,Q)

    def update(self, var_sensors, rate_sensors, input=None):
        active_sensors = array( [int(x is not None) for x in var_sensors]
                               +[int(x is not None) for x in rate_sensors] ).reshape(self.m, 1)
        var_sensors = [0 if x is None else x for x in var_sensors]
        rate_sensors = [0 if x is None else x for x in rate_sensors]

        z = np.concatenate( [var_sensors, rate_sensors] )

        # If we're measuring it mod 360, then we find the closest values to 360
        # and use those rather than the original measurements
        if self.mod360:
            z[:self.n_var] = [self.xHat[i] + diff(self.xHat[i], z[i]) for i in range(self.n_var)]

        if input is None:
            u = np.zeros((self.n,))
        else:
            u = array(input).reshape( (self.n, ) )

        xHat, P = self.Iterate(u, z, active_measurements=active_sensors)
        var = xHat[0]
        rate = xHat[1]
        biases = xHat[2:]
        return var,rate,biases

def to180(d):
    if d > 180.:
        return d-360.
    else:
        return d
def diff(x, y):
    return to180( (y-x)%360. )
