'''utility functions for working with data in the log plotter'''

import scipy.integrate
import scipy.ndimage
import numpy as np

def hdg(array):
    '''Converts an array to a 0-360 heading starting at 0'''
    return (array-array[0])%360.

def make_funcs(variables):
    '''Simplifies some functions through currying the time axis'''

    def integrate(y):
        '''numeric integration of ys'''
        return scipy.integrate.cumtrapz(y, x=variables["t"])

    def differentiate(y, sigma=5):
        '''numeric differentiation of xs.
        fudges it so that the length of output is the same as the input'''

        time = variables["t"]
        y = scipy.ndimage.gaussian_filter1d(y, sigma=sigma)
        diff = (y[1:]-y[:-1])/(time[1:]-time[:-1])
        return np.concatenate(([diff[0]], diff))

    return dict(integrate=integrate, differentiate=differentiate, hdg=hdg)

#Other funcs
def corr(array):
    cov = np.cov(array)
    sigma = np.sqrt( cov.diagonal() )
    sigma = sigma[:,np.newaxis]
    return np.abs(cov / (sigma*sigma.T))

