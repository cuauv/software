import queue
import time

try:
    import cupy as xp
except ImportError:
    import numpy as xp
import numpy as np
from scipy.interpolate import interp1d

from common import const, crop, plot

class GainPlot(plot.PlotBase):
    def plot(self, sig, gains):
        L_interval = sig.shape[1]

        peak_pos = int(xp.abs(sig).argmax() % L_interval)
        (plot_start, plot_end) = crop.find_bounds(
            L_interval, const.L_GAIN_PLOT, peak_pos)
        cursor_pos = peak_pos - plot_start

        sig = sig[:, plot_start : plot_end]
        gains = gains[:, plot_start : plot_end]

        if hasattr(xp, 'asnumpy'):
            sig = xp.asnumpy(sig)
            gains = xp.asnumpy(gains)
        try:
            self._q.put_nowait((sig, gains, cursor_pos))
        except queue.Full:
            pass

    @staticmethod
    def _daemon(q):
        (pyplot, fig) = plot.PlotBase._daemon_init()

        orig_indices = np.arange(0, const.L_GAIN_PLOT)
        interp_indices = np.linspace(0, const.L_GAIN_PLOT - 1, num=1000)

        pyplot.suptitle('Gain Plot')
        (_, sig_lines, sig_cursor) = (
            GainPlot._define_sig_plot(fig, interp_indices))
        (_, gains_lines, gains_cursor) = (
            GainPlot._define_gains_plot(fig, orig_indices))

        while True:
            try:
                (sig, gains, cursor_pos) = q.get_nowait()

                sig = interp1d(orig_indices, sig, kind='cubic')(interp_indices)
                for ch_num in range(sig.shape[0]):
                    sig_lines[ch_num].set_ydata(sig[ch_num])
                sig_cursor.set_xdata(cursor_pos)

                gains_lines[0].set_ydata(gains[0])
                gains_cursor.set_xdata(cursor_pos)

                pyplot.draw()
                pyplot.show(block=False)
            except queue.Empty:
                pass

            fig.canvas.flush_events()
            time.sleep(const.GUI_UPDATE_TIME)

    @staticmethod
    def _define_sig_plot(fig, indices):
        ax = fig.add_subplot(211)
        ax.set_ylabel('Raw Signal')
        ax.set_xticks(np.arange(0, const.L_GAIN_PLOT, const.L_GAIN_PLOT // 10))
        ax.set_xlim(0, const.L_GAIN_PLOT - 1)
        ax.set_ylim(-const.BIT_DEPTH // 2, const.BIT_DEPTH // 2 - 1)
        lines = ax.plot(indices, np.zeros(indices.shape), 'r-',
                        indices, np.zeros(indices.shape), 'g-',
                        indices, np.zeros(indices.shape), 'b-',
                        indices, np.zeros(indices.shape), 'm-',
                        linewidth=0.5)
        cursor = ax.axvline(x=0, color='red', linestyle=':')
        return (ax, lines, cursor)

    @staticmethod
    def _define_gains_plot(fig, indices):
        ax = fig.add_subplot(212)
        ax.set_xlabel('Sample Number')
        ax.set_ylabel('Gain')
        ax.set_yscale('log')
        ax.set_xticks(np.arange(0, const.L_GAIN_PLOT, const.L_GAIN_PLOT // 10))
        ax.set_xlim(0, const.L_GAIN_PLOT - 1)
        ax.set_ylim(0.9, 200)
        lines = ax.plot(indices, np.ones(indices.shape), 'k-',
                        linewidth=0.5)
        cursor = ax.axvline(x=0, color='red', linestyle=':')
        return (ax, lines, cursor)