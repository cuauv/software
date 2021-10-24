import math
import queue
import time

import numpy as np

from common import const, plot

class ScatterPlot(plot.PlotBase):
    def plot(self, hdg, elev):
        try:
            self._q.put_nowait((hdg, elev))
        except queue.Full:
            pass

    @staticmethod
    def _daemon(q):
        from matplotlib.ticker import AutoMinorLocator

        (pyplot, fig) = plot.PlotBase._daemon_init()

        pyplot.suptitle('Relative Heading/Elevation Scatter Plot')
        (ax, points, text) = ScatterPlot._define_plot(fig, AutoMinorLocator)

        hdg_list = []
        elev_list = []
        while True:
            try:
                (hdg, elev) = q.get_nowait()
                hdg_list.append(hdg)
                elev_list.append(elev)

                points.set_data(hdg_list, elev_list)
                text.set_text(
                    'HDG std: ' + '{:.2f}'.format(np.std(hdg_list)) + '\n' +
                    'HDG mean: ' + '{:.2f}'.format(np.mean(hdg_list)) + '\n' +
                    'ELEV std: ' + '{:.2f}'.format(np.std(elev_list)) + '\n' +
                    'ELEV mean: ' + '{:.2f}'.format(np.mean(elev_list)))

                pyplot.draw()
                pyplot.show(block=False)
            except queue.Empty:
                pass

            fig.canvas.flush_events()
            time.sleep(const.GUI_UPDATE_TIME)

    @staticmethod
    def _define_plot(fig, AutoMinorLocator):
        ax = fig.add_subplot(111)
        ax.set_xlabel('Heading (rad)')
        ax.set_ylabel('Elevation (rad)')
        ax.set_xticks([-3, -2, -1, 0, 1, 2, 3])
        ax.set_yticks([-1, 0, 1])
        ax.xaxis.set_minor_locator(AutoMinorLocator(10))
        ax.yaxis.set_minor_locator(AutoMinorLocator(10))
        ax.set_xlim(-math.pi, math.pi)
        ax.set_ylim(-math.pi / 2, math.pi / 2)
        ax.set_aspect('equal', adjustable='box')
        ax.grid(True, which='major', linestyle='-')
        ax.grid(True, which='minor', linestyle=':')
        points = ax.plot([], [], color='r', marker='.', ls='', markersize=1)
        text = ax.text(-3, 1.6, '')
        return (ax, points[0], text)