#!/usr/bin/env python2
'''Uses wxPython and matplotlib to plot in real-time shared memory values'''

import wx
import matplotlib as mpl
import numpy as np
import math
import shm

from matplotlib.projections import PolarAxes, register_projection
from matplotlib.transforms import Affine2D, Bbox, IdentityTransform



#radial distance to plot to in meters
r_dist = 6


#Currently, it is necessary to modify any variables you want here
#I should probably make this in the gui


#simulated hydrophone absolute heading shared var
class HydroHdg:
    def get(self):
        return shm.hydrophones_results.heading.get()

    #approximates lateral distance to hydrophones based on elevation + altitude
    #assumes: ground is level; pinger is on ground
    def get_distance(self):
        #dvlalt = SharedVar("/sensors/dvl/savg_altitude").get()
        #if dvlalt <= 0: return r_dist
        #elev = SharedVar("/sensors/hydrophones/elevation").get()
        #if elev >= 90: return r_dist
        #dist = dvlalt * math.tan(math.radians(elev))
        #if dist > r_dist: return r_dist
        # return dist
        return 0
        

hhdg = HydroHdg()


variables = {
            "him" : shm.him.heading,
            "gx4" : shm.gx4.heading,
            "gx1" : shm.threedmg.heading
            }

enables = dict()
styles = [ "r-", "r-.", "b-", "y-", "r--", "b--", "g--", "y--"]

sample_interval = 0.1

mpl.interactive(True)
mpl.use('WXAgg')

import pylab

#Transform to allow for clockwise plotting w/ 0* pointing up.
class NorthPolarAxes(PolarAxes):
    '''
    A variant of PolarAxes where theta starts pointing north and goes
    clockwise.
    '''
    name = 'northpolar'

    class NorthPolarTransform(PolarAxes.PolarTransform):
        def transform(self, tr):
            xy   = np.zeros(tr.shape, np.float_)
            t    = tr[:, 0:1]
            r    = tr[:, 1:2]
            x    = xy[:, 0:1]
            y    = xy[:, 1:2]
            x[:] = r * np.sin(t)
            y[:] = r * np.cos(t)
            return xy

        transform_non_affine = transform

        def inverted(self):
            return NorthPolarAxes.InvertedNorthPolarTransform()

    class InvertedNorthPolarTransform(PolarAxes.InvertedPolarTransform):
        def transform(self, xy):
            x = xy[:, 0:1]
            y = xy[:, 1:]
            r = np.sqrt(x*x + y*y)
            theta = np.arctan2(y, x)
            return np.concatenate((theta, r), 1)

        def inverted(self):
            return NorthPolarAxes.NorthPolarTransform()

    def _set_lim_and_transforms(self):
        PolarAxes._set_lim_and_transforms(self)
        self.transProjection = self.NorthPolarTransform()
        self.transData = (
            self.transScale + 
            self.transProjection + 
            (self.transProjectionAffine + self.transAxes))
        self._xaxis_transform = (
            self.transProjection +
            self.PolarAffine(IdentityTransform(), Bbox.unit()) +
            self.transAxes)
        self._xaxis_text1_transform = (
            self._theta_label1_position +
            self._xaxis_transform)
        self._yaxis_transform = (
            Affine2D().scale(np.pi * 2.0, 1.0) +
            self.transData)
        #self._yaxis_text1_transform = (
        #    self._r_label1_position +
        #    Affine2D().scale(1.0 / 360.0, 1.0) +
        #    self._yaxis_transform)

register_projection(NorthPolarAxes)


class PlotPanel( wx.Panel ):
    def __init__(self, parent, dpi = None, color=None, *args, **kwargs):
        from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg
        wx.Panel.__init__(self, parent, wx.ID_ANY, *args, **kwargs)

        self.figure = mpl.figure.Figure( None, dpi )
        self.canvas = FigureCanvasWxAgg(self, -1, self.figure)

        self._SetSize()

        self.make()
        self.draw()
        self._resizeflag = False
        self.Bind(wx.EVT_IDLE, self._onIdle)
        self.Bind(wx.EVT_SIZE, self._onSize)

    def _SetSize(self):
        pixels = tuple(self.parent.GetClientSize())
        self.SetSize(pixels)
        self.canvas.SetSize(pixels)
        self.figure.set_size_inches( float(pixels[0])/self.figure.get_dpi(),
                                     float(pixels[1])/self.figure.get_dpi() )
    def _onSize(self,event):
        self._resizeflag = True

    def _onIdle(self,event):
        if self._resizeflag:
            self._resizeflag = False
            self._SetSize()

    def draw(self):
        pass # To be overriden by children
    def make(self):
        pass # To be overriden by children

class Plot(PlotPanel):
    def __init__(self, parent, *args, **kwargs ):
        self.parent = parent
        self.data = [[0,r_dist] for key in variables.keys()]

        PlotPanel.__init__(self, parent, *args, **kwargs)

    def make(self):
        self.axes = self.figure.add_subplot(111, projection='northpolar')
        self.plot_data = [self.axes.plot(self.data[i], styles[i%len(styles)], label=key)[0] for i,key in enumerate(variables.keys())]

        #hydrophone distance plot
        self.hydro_dot = self.axes.plot(math.radians(hhdg.get()), hhdg.get_distance(), 'bo')[0]
        

        self.redraw_timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.draw, self.redraw_timer)
        self.redraw_timer.Start(1000*sample_interval)

        self.legend = self.axes.legend(loc="upper left")

        # r labels good for hydro_dot
        #for label in self.axes.get_yticklabels():
        #    label.set_visible(False)

    def ToggleLegend(self, evt=None):
        self.legend.set_visible(not self.legend.get_visible())

    def draw(self, event=None):
        for i,key in enumerate(variables.keys()):
            if not enables[key]:
                #Don't plot it if its not enabled
                self.plot_data[i].set_data([],[])
                self.plot_data[i].set_label("_nolegend_")
                continue
        
            self.plot_data[i].set_label(key)
        
            self.data[i] = [math.radians(variables[key].get()), math.radians(variables[key].get())]

            rdata = [0,r_dist]
            self.plot_data[i].set_data(self.data[i], rdata)

        #self.hydro_dot.set_xdata(math.radians(hhdg.get()))
        #self.hydro_dot.set_ydata(hhdg.get_distance())

        if( self.legend == None or self.legend.get_visible() ):
            self.legend = self.axes.legend(loc="upper left")

app = wx.PySimpleApp(False)

def Startup():
    global enables
    enables = dict([(var, True) for var in variables.keys()])
    frame = wx.Frame(None, wx.ID_ANY, "Heading Plotter", size=(450,450))
            
    #Make the menu
    menubar = wx.MenuBar()
    frame.SetMenuBar(menubar)

    panel = Plot(frame)

    #Make variable toggle menu
    variableMenu = wx.Menu()
    def ToggleVar(var):
        def func(event):
            enables[var] = not enables[var]
        return func
    #Make toggle buttons
    varBtns = [variableMenu.Append(wx.ID_ANY, var, var, kind=wx.ITEM_CHECK) for i,var in enumerate(variables.keys())]
    [frame.Bind(wx.EVT_MENU, ToggleVar(var), varBtns[i]) for i,var in enumerate(variables.keys())]
    [btn.Toggle() for btn in varBtns]
    menubar.Append(variableMenu, "Variables")

    #Settings Menu
    settingsMenu = wx.Menu()
    legendToggleBtn = settingsMenu.Append(wx.ID_ANY, "Legend", "Legend", kind=wx.ITEM_CHECK)
    frame.Bind(wx.EVT_MENU, panel.ToggleLegend, legendToggleBtn)
    legendToggleBtn.Toggle()
    menubar.Append(settingsMenu, "Settings")

    frame.Show(True)

Startup()
app.MainLoop()
