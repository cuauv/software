try:
    import cupy as xp
except ImportError:
    import numpy as xp

from common import const, gainplot, pack
from common.hardware import HydrophonesSection
try:
    import shm
except ImportError:
    from common import shm

class Controller:
    def __init__(self, section, L_interval, plot=False):
        if section is HydrophonesSection.PINGER:
            self._shm_settings = shm.hydrophones_pinger_settings
            self._shm_settings.gain_control_mode.set(1)
        else:
            assert section is HydrophonesSection.COMMS, (
                'Hydrophones board has two sections, PINGER and COMMS')
            self._shm_settings = shm.hydrophones_comms_settings
            self._shm_settings.gain_control_mode.set(2)
        self._shm_settings.user_gain_lvl.set(13)

        self._plot = gainplot.GainPlot() if plot else None

        self._gain_values_array = xp.array(const.GAIN_VALUES)

        self._sig_pkr = pack.Packer(L_interval)
        self._gains_pkr = pack.Packer(L_interval)

    def push(self, sig, gains):
        packed_sig = self._sig_pkr.push(sig)
        packed_gains = self._gains_pkr.push(gains)

        if packed_sig is not None:
            if self._plot is not None:
                self._plot.plot(packed_sig, packed_gains)

            gain_ctrl_mode = self._shm_settings.gain_control_mode.get()
            if gain_ctrl_mode == 0:
                return (False, self._shm_settings.user_gain_lvl.get())
            elif gain_ctrl_mode == 1:
                return (False, self._best_gain_lvl(packed_sig, packed_gains))
            else:
                return (True, 0)

        return None

    def _best_gain_lvl(self, sig, gains):
        peak = xp.abs(sig).max()
        peak_pos = xp.abs(sig).argmax() % sig.shape[1]
        gain_at_peak = gains[:, peak_pos]

        peak_for_gains = peak / gain_at_peak * self._gain_values_array

        desire = int((peak_for_gains < const.CLIPPING_THRESHOLD).sum() - 1)

        return desire if desire > 0 else 0