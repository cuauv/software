import cmath

try:
    import cupy as xp
except ImportError:
    import numpy as xp

class Mixer:
    def __init__(self, L_x, ph=0, w=0):
        assert L_x >= 1, 'Input block length must be at least 1'

        self._L_x = L_x
        self._ph = ph

        self.set_freq(w)

    def push(self, x):
        nco = cmath.exp(1j * self._ph) * self._offsets
        self._ph += self._w * self._L_x

        y = x * nco

        return y

    def set_freq(self, w):
        self._offsets = xp.exp(1j * w * xp.arange(self._L_x))
        self._w = w

    def get_freq(self):
        return self._w