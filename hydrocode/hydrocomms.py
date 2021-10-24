import os
import queue
import sys
from threading import Thread

try:
    import cupy as xp
    print('Using CuPy\n')
except ImportError:
    import numpy as xp

sys.path.insert(0, os.path.dirname(__file__) + '/modules')
from common import convert, downconv, filt, gain, hardware
import common.const
from comms import demodulate, synchronize
import comms.const
try:
    import shm
except ImportError:
    from common import shm

class Receive:
    def __init__(self, q, gain_plot=False, correlation_plot=False, dump=False):
        self._daemon_thread = Thread(target=self._daemon,
            args=(q, gain_plot, correlation_plot, dump), daemon=True)
        self._daemon_thread.start()

    @staticmethod
    def _daemon(q, gain_plot, correlation_plot, dump):
        gainctrl = gain.Controller(
            hardware.HydrophonesSection.COMMS,
            int(comms.const.DUR_GAIN_INTERVAL * common.const.SAMPLE_RATE),
            plot=gain_plot,
        )

        num_symbols = 2 ** comms.const.SYMBOL_SIZE
        symbol_spacing = comms.const.BANDWIDTH / (num_symbols - 1)
        symbol_stopband = comms.const.SYMBOL_STOPBAND_FRAC * 2 * symbol_spacing
        h = filt.firgauss(
            convert.omega_hat(symbol_stopband),
            comms.const.FIR_ORDER,
        )
        symbols = (comms.const.FREQUENCY - comms.const.BANDWIDTH / 2 +
            xp.arange(num_symbols) * symbol_spacing)
        dwncnvs = []
        for symbol in symbols:
            dwncnvs.append(
                downconv.Downconverter(
                    4 if common.const.USE_4CHS else 3,
                    comms.const.L_FIR_BLOCK,
                    h,
                    D=comms.const.DECIM_FACTOR,
                    w=(convert.omega_hat(symbol)),
                )
            )

        sync = synchronize.Synchronizer(
            comms.const.MSG_BYTES * 8 // comms.const.SYMBOL_SIZE + 1,
            comms.const.L_SYM,
            comms.const.PN_SEQ,
            comms.const.ORTH_SEQ,
            plot=correlation_plot,
        )

        hydrobrd = hardware.HydrophonesBoard(
            hardware.HydrophonesSection.COMMS,
            common.const.PKTS_PER_RECV,
            dump=dump,
        )

        while True:
            (sig, gains, _) = hydrobrd.receive()

            gainctrl_result = gainctrl.push(sig, gains)
            if gainctrl_result is not None:
                (brd_ag, gain_lvl_desire) = gainctrl_result
                hydrobrd.config(autogain=brd_ag, man_gain_lvl=gain_lvl_desire)

            sig = sig / gains

            symbol_sigs = []
            for symbol_num in range(num_symbols):
                symbol_sig = dwncnvs[symbol_num].push(sig)
                if symbol_sig is not None:
                    symbol_sigs.append(xp.abs(symbol_sig).sum(axis=0))
            if len(symbol_sigs) == num_symbols:
                symbol_sigs = sync.push(xp.stack(symbol_sigs))
                if symbol_sigs is not None:
                    syms = demodulate.decide(symbol_sigs, comms.const.L_SYM)
                    msg = demodulate.decode(syms[1 :], comms.const.SYMBOL_SIZE)
                    try:
                        q.put_nowait(msg)
                    except queue.Full:
                        print('\nComms Rx queue full. Lost transmission.\n')

class Transmit:
    def __init__(self, q):
        self._daemon_thread = Thread(target=self._daemon,
            args=(q,), daemon=True)
        self._daemon_thread.start()

    @staticmethod
    def _daemon(q):
        num_symbols = 2 ** comms.const.SYMBOL_SIZE
        head = comms.const.PN_SEQ + [-1]
        head_symbols = (xp.asarray(head) == 1) * (num_symbols - 1)
        head_bytes = demodulate.decode(head_symbols, comms.const.SYMBOL_SIZE)

        transbrd = hardware.TransmitBoard()

        while True:
            msg = q.get()
            if type(msg) is not bytes:
                raise TypeError('Expected bytes object, got ' + str(type(msg)))
            if len(msg) != comms.const.MSG_BYTES:
                raise ValueError('Message size must be ' +
                    str(comms.const.MSG_BYTES) + ' bytes')

            transbrd.send(head_bytes + msg)