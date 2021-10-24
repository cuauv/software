try:
    import cupy as xp
except ImportError:
    import numpy as xp

from common import filt, pack
from comms import const, corrplot

class Synchronizer:
    def __init__(self, L_msg, L_sym, pn_seq, orth_seq, plot=False):
        assert L_sym >= 1, 'There must be at least one sample per symbol'

        assert len(pn_seq) >= 1, (
            'The PN sequence must have at least one symbol')
        assert len(pn_seq) == len(orth_seq), (
            'The PN/orthogonal sequence lengths must be equal')

        assert L_msg >= len(pn_seq), (
            'The message must be at least as long as the PN sequence')

        self._L_pn_samples = len(pn_seq) * L_sym
        self._L_msg_samples = L_msg * L_sym
        L_transmission_samples = self._L_pn_samples + self._L_msg_samples

        h = xp.flip(xp.asarray(pn_seq, dtype=float).repeat(L_sym), axis=0)
        h /= len(h)
        self._pn_correlator = filt.FIR(1, L_transmission_samples - 1, h)

        h = xp.flip(xp.asarray(orth_seq, dtype=float).repeat(L_sym), axis=0)
        h /= len(h)
        self._orth_correlator = filt.FIR(1, L_transmission_samples - 1, h)

        h = xp.ones(const.L_THRESH_CALC) * const.THRESHOLD_FACTOR ** 2
        h /= len(h)
        self._thresh_accum = filt.FIR(1, L_transmission_samples - 1, h)

        self._plot = corrplot.CorrelationPlot() if plot else None

        self._input_pkr = pack.Packer(L_transmission_samples - 1)
        self._transmission_pkr = pack.Packer(L_transmission_samples)

        self._triggered = False

    def push(self, x):
        packed = self._input_pkr.push(x)
        if packed is not None:
            return self._sync(packed)

        return None

    def _sync(self, x):
        msg = None

        corr_in = (x[-1] - x[0]).reshape(1, -1)
        corr_pn = self._pn_correlator.push(corr_in).real
        corr_orth = self._orth_correlator.push(corr_in).real
        thresh = xp.sqrt(self._thresh_accum.push(corr_orth ** 2).real)
        stacked = xp.concatenate((corr_in, corr_pn, corr_orth, thresh, x))

        if self._triggered:
            transmission = self._transmission_pkr.push(stacked)
            if transmission is not None:
                (msg, stacked_r1) = self._extract_msg(transmission)
                stacked_r2 = self._transmission_pkr.get()
                stacked = xp.concatenate((stacked_r1, stacked_r2), axis=1)
                self._transmission_pkr.reset()
                self._triggered = False

                if self._plot is not None:
                    self._plot.plot(transmission[0].reshape(1, -1),
                                    transmission[1].reshape(1, -1),
                                    transmission[2].reshape(1, -1),
                                    transmission[3].reshape(1, -1))

        if not self._triggered:
            trig = xp.logical_and(stacked[1] >= const.SQUELCH_THRESH,
                                  stacked[1] >= stacked[3])
            if trig.any():
                self._transmission_pkr.push(stacked[:, trig.argmax() :])
                self._triggered = True

        return msg

    def _extract_msg(self, transmission):
        msg_start = int(transmission[1, : self._L_pn_samples].argmax() + 1)
        msg_end = msg_start + self._L_msg_samples

        msg = transmission[4 :, msg_start : msg_end]
        remainder = transmission[:, msg_end :]

        return (msg, remainder)