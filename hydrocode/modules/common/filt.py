import math

try:
    import cupy as xp
except ImportError:
    import numpy as xp
from scipy.signal import firwin, kaiserord, windows

class FIR:
    def __init__(self, num_chs, L_x, h, D=1):
        assert num_chs >= 1, 'Filter must have at least one input channel'

        assert h.ndim == 1, 'Impulse response must be a 1D array'
        assert len(h) - 1 >= 0, 'FIR Order must be at least 0'

        assert D >= 1, 'Decimation factor must be at least 1'

        assert (len(h) - 1) % D == 0, (
            'FIR order must be a multiple of the decimation factor')
        assert L_x >= 1, 'Input block length must be at least 1'
        assert L_x % D == 0, (
            'Input block length must be a multiple of the decimation factor')

        self._num_chs = num_chs
        self._L_x = L_x
        self._D = D

        self._overlap_samples = xp.zeros((num_chs, len(h) - 1), dtype=complex)

        self._L_ifft = (L_x + len(h) - 1) // D
        self._L_transient = (len(h) - 1) // D
        self._H = xp.fft.fft(h, n=(L_x + len(h) - 1))

    def push(self, x):
        x = xp.concatenate((self._overlap_samples, x), axis=1)
        X = xp.fft.fft(x)

        Y = X * self._H
        Y = Y.reshape(self._num_chs, -1, self._L_ifft).sum(axis=1) / self._D
        y = xp.fft.ifft(Y)
        y = y[:, self._L_transient :]

        self._overlap_samples = x[:, self._L_x :]

        return y

def firkaiser(passband, stopband, atten=60):
    (L_filt, beta) = kaiserord(atten, (stopband - passband) / (2 * math.pi))
    h = firwin(L_filt, passband / 2, window=('kaiser', beta), nyq=math.pi)
    h = xp.asarray(h)

    return h

def firgauss(stopband, order, atten=60):
    assert stopband > 0, 'Stopband must be greater than 0'
    assert order >= 0, 'Order must be at least 0'
    assert atten > 0, 'Attenuation at stopband must be greater than 0'

    std = 2 * math.sqrt(atten / 10 * math.log(10)) / stopband

    h = xp.asarray(windows.gaussian(order + 1, std))
    h /= h.sum()

    return h

def gauss_rise_time(h):
    low = 0.01
    high = 0.99

    assert len(h) - 1 >= 0, 'FIR Order must be at least 0'

    step_resp = h.cumsum()
    step_resp /= step_resp[-1]

    rise_t = int(xp.abs(step_resp - high).argmin() -
        xp.abs(step_resp - low).argmin())

    return rise_t