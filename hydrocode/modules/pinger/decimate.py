from common import pack

class Decimator:
    def __init__(self, L_b, D=1):
        assert L_b % D == 0, (
            'Decimation block length must be a multiple of the dec. factor')

        self._D = D

        self._pkr = pack.Packer(L_b)

    def push(self, x):
        packed = self._pkr.push(x)
        if packed is not None:
            return packed[:, : : self._D]

        return None