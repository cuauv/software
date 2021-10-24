from common import filt, mix, pack

class Downconverter:
    def __init__(self, num_chs, L_b, h, D=1, ph=0, w=0):
        self._mixr = mix.Mixer(L_b, ph=ph, w=w)
        self._filtr = filt.FIR(num_chs, L_b, h, D=D)

        self._pkr = pack.Packer(L_b)

    def push(self, x):
        packed = self._pkr.push(x)
        if packed is not None:
            return self._filtr.push(self._mixr.push(packed))

        return None

    def set_freq(self, w):
        self._mixr.set_freq(w)

    def get_freq(self):
        return self._mixr.get_freq()