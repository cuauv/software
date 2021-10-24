import math
import queue
import time

from common import const, plot

class HeadingPlot(plot.PlotBase):
    def plot(self, hdg, elev):
        try:
            self._q.put_nowait((hdg, elev))
        except queue.Full:
            pass

    @staticmethod
    def _daemon(q):
        from matplotlib import transforms

        (pyplot, fig) = plot.PlotBase._daemon_init()

        pyplot.suptitle('Relative Heading Plot')
        pointy = pyplot.imread('img/pointy.png')
        (x_center, y_center) = (pointy.shape[0] // 2, pointy.shape[1] // 2)
        (ax, im, text) = HeadingPlot._define_plot(fig, pointy)

        while True:
            try:
                (hdg, elev) = q.get_nowait()

                trans_data = transforms.Affine2D().rotate_around(
                    x_center, y_center, hdg)
                im.set_transform(trans_data + ax.transData)
                text.set_text(
                    '(Elevation: ' + str(int(math.degrees(elev))) + '°)')

                pyplot.draw()
                pyplot.show(block=False)
            except queue.Empty:
                pass

            fig.canvas.flush_events()
            time.sleep(const.GUI_UPDATE_TIME)

    @staticmethod
    def _define_plot(fig, img):
        ax0 = fig.add_subplot(111, label='pointy')
        ax0.axis('off')
        im = ax0.imshow(img)
        text = ax0.text(0, 0, '')

        ax1 = fig.add_subplot(111, polar=True, label='deg')
        ax1.set_yticklabels([])
        ax1.set_theta_zero_location('N')
        ax1.set_theta_direction(-1)
        ax1.set_facecolor('None')

        return (ax0, im, text)