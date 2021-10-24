import math
import os

from common import const
from pinger import headingplot, scatterplot

class AnglesMLE:
    def __init__(self, heading_plot=False, scatter_plot=False):
        self._heading_plot = (
            headingplot.HeadingPlot() if heading_plot else None)
        self._scatter_plot = (
            scatterplot.ScatterPlot() if scatter_plot else None)

    def hdg_elev(self, ping_phase, w):
        d = list(self._path_diff(wrap_angle(ping_phase - ping_phase[0]), w))

        v = math.sqrt(d[1] ** 2 + d[2] ** 2)
        hdg = 0 if v == 0 else -math.atan2(d[2], d[1])

        if const.USE_4CHS:
            elev = 0 if v == 0 and d[3] == 0 else math.atan2(d[3], v)
        else:
            elev = 0 if v > 1 else -math.acos(v)

        if (os.environ['CUAUV_VEHICLE_TYPE'] == "mainsub"):
            hdg -= math.pi / 2
        else:
            pass
        hdg = wrap_angle(hdg)

        if self._heading_plot is not None:
            self._heading_plot.plot(hdg, elev)

        if self._scatter_plot is not None:
            self._scatter_plot.plot(hdg, elev)

        return (hdg, elev)

    @staticmethod
    def _path_diff(ph, w):
        return const.SOUND_SPEED * ph / (const.NIPPLE_DIST *
            const.SAMPLE_RATE * w)

def wrap_angle(theta):
    return (theta + math.pi) % (2 * math.pi) - math.pi