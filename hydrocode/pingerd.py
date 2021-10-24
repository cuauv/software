#!/usr/bin/env python3

import sys

try:
    import cupy as xp
    print('Using CuPy\n')
except ImportError:
    import numpy as xp

sys.path.insert(0, 'modules')
from common import convert, downconv, filt, gain, hardware
import common.const
from pinger import angles, decimate, trigger
import pinger.const
try:
    import shm
except ImportError:
    from common import shm

if __name__ == '__main__':
    print('Pingerd starting...')

    L_INTERVAL = int(pinger.const.DUR_INTERVAL * common.const.SAMPLE_RATE)

    gainctrl = gain.Controller(
        hardware.HydrophonesSection.PINGER,
        L_INTERVAL,
        plot=('-gain_plot' in sys.argv),
    )

    h = filt.firgauss(
        convert.omega_hat(pinger.const.STOPBAND),
        pinger.const.FIR_ORDER,
    )
    freq = shm.hydrophones_pinger_settings.frequency.get()
    if freq not in pinger.const.USUAL_FREQS:
        print("Warning: Tracking unusual frequency " + str(freq) + " Hz")
    dwncnv = downconv.Downconverter(
        4 if common.const.USE_4CHS else 3,
        pinger.const.L_FIR_BLOCK,
        h,
        D=pinger.const.DECIM_FACTOR,
        w=(convert.omega_hat(freq)),
    )

    subhdgsdecim = decimate.Decimator(
        pinger.const.L_FIR_BLOCK,
        D=pinger.const.DECIM_FACTOR,
    )

    fir_rise_time = filt.gauss_rise_time(h)
    trig = trigger.Trigger(
        L_INTERVAL // pinger.const.DECIM_FACTOR,
        pinger.const.L_SEARCH,
        fir_rise_time // pinger.const.DECIM_FACTOR,
        trigger_plot=('-trigger_plot' in sys.argv),
        ping_plot=('-ping_plot' in sys.argv),
    )

    anglmle = angles.AnglesMLE(
        heading_plot=('-heading_plot' in sys.argv),
        scatter_plot=('-scatter_plot' in sys.argv)
    )

    hydrobrd = hardware.HydrophonesBoard(
        hardware.HydrophonesSection.PINGER,
        common.const.PKTS_PER_RECV,
        dump=('-dump' in sys.argv),
    )

    while True:
        (sig, gains, sub_hdgs) = hydrobrd.receive()

        gainctrl_result = gainctrl.push(sig, gains)
        if gainctrl_result is not None:
            (brd_ag, gain_lvl_desire) = gainctrl_result
            hydrobrd.config(autogain=brd_ag, man_gain_lvl=gain_lvl_desire)

        sig = sig / gains

        sig = dwncnv.push(sig)
        sub_hdgs = subhdgsdecim.push(sub_hdgs)
        if sig is not None:
            trig_result = trig.push(sig, sub_hdgs)
            if trig_result is not None:
                (ping_phase, sub_hdg) = trig_result
                (hdg, elev) = anglmle.hdg_elev(ping_phase, dwncnv.get_freq())
                abs_hdg = angles.wrap_angle(sub_hdg + hdg)
                shm.hydrophones_pinger_results.heading.set(abs_hdg)
                shm.hydrophones_pinger_results.elevation.set(elev)
                print('Relative HDG: ' + '{:.2f}'.format(hdg) +
                    ' Relative ELEV: ' + '{:.2f}'.format(elev))

                dwncnv.set_freq(convert.omega_hat(
                    shm.hydrophones_pinger_settings.frequency.get()))