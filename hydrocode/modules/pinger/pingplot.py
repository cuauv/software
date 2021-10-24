import queue
import time

try:
    import cupy as xp
except ImportError:
    import numpy as xp
import numpy as np
from scipy.interpolate import interp1d

from common import crop, plot
import common.const
import pinger.const

class PingPlot(plot.PlotBase):
    def plot(self, x, ping_pos):
        L_interval = x.shape[1]

        (plot_start, plot_end) = crop.find_bounds(
            L_interval, pinger.const.L_PING_PLOT, ping_pos)
        cursor_pos = ping_pos - plot_start

        x = x[:, plot_start : plot_end]

        if hasattr(xp, 'asnumpy'):
            x = xp.asnumpy(x)
        try:
            self._q.put_nowait((x, cursor_pos))
        except queue.Full:
            pass

    @staticmethod
    def _daemon(q):
        (pyplot, fig) = plot.PlotBase._daemon_init()

        interp_indices = np.linspace(0, pinger.const.L_PING_PLOT - 1, num=1000)

        pyplot.suptitle('Ping Plot')
        (ampl_ax, ampl_lines, ampl_cursor) = (
            PingPlot._define_ampl_plot(fig, interp_indices))
        (phase_ax, phase_lines, phase_cursor) = (
            PingPlot._define_phase_plot(fig, interp_indices))

        while True:
            try:
                (x, cursor_pos) = q.get_nowait()

                x = PingPlot._interp_complex(x, interp_indices)

                ampl = np.abs(x)
                plot.PlotBase._auto_ylim(ampl_ax, ampl)
                for ch_num in range(ampl.shape[0]):
                    ampl_lines[ch_num].set_ydata(ampl[ch_num])
                ampl_cursor.set_xdata(cursor_pos)

                phase = np.angle(x)
                for ch_num in range(phase.shape[0]):
                    phase_lines[ch_num].set_ydata(phase[ch_num])
                phase_cursor.set_xdata(cursor_pos)

                pyplot.draw()
                pyplot.show(block=False)
            except queue.Empty:
                pass

            fig.canvas.flush_events()
            time.sleep(common.const.GUI_UPDATE_TIME)

    @staticmethod
    def _define_ampl_plot(fig, indices):
        ax = fig.add_subplot(211)
        ax.set_ylabel('Signal Amplitude')
        ax.set_xticks(np.arange(
            0, pinger.const.L_PING_PLOT, pinger.const.L_PING_PLOT // 10))
        ax.set_xlim(0, pinger.const.L_PING_PLOT - 1)
        lines = ax.plot(indices, np.zeros(indices.shape), 'r-',
                        indices, np.zeros(indices.shape), 'g-',
                        indices, np.zeros(indices.shape), 'b-',
                        indices, np.zeros(indices.shape), 'm-',
                        linewidth=0.5)
        cursor = ax.axvline(x=0, color='red', linestyle=':')
        return (ax, lines, cursor)

    @staticmethod
    def _define_phase_plot(fig, indices):
        ax = fig.add_subplot(212)
        ax.set_xlabel('Decimated Sample Number')
        ax.set_ylabel('Signal Phase')
        ax.set_xticks(np.arange(
            0, pinger.const.L_PING_PLOT, pinger.const.L_PING_PLOT // 10))
        ax.set_xlim(0, pinger.const.L_PING_PLOT - 1)
        ax.set_ylim(-4, 4)
        lines = ax.plot(indices, np.zeros(indices.shape), 'r-',
                        indices, np.zeros(indices.shape), 'g-',
                        indices, np.zeros(indices.shape), 'b-',
                        indices, np.zeros(indices.shape), 'm-',
                        linewidth=0.5)
        cursor = ax.axvline(x=0, color='red', linestyle=':')
        return (ax, lines, cursor)

    @staticmethod
    def _interp_complex(x, interp_indices):
        orig_indices = np.arange(0, pinger.const.L_PING_PLOT)

        real = x.real
        real = interp1d(orig_indices, real, kind='cubic')(interp_indices)

        imag = x.imag
        imag = interp1d(orig_indices, imag, kind='cubic')(interp_indices)

        return real + 1j * imag