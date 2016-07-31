#!/usr/bin/env python2
'''Uses wxPython and matplotlib to plot in real-time shared memory values
NOW IN 3D!!'''

import wx
import matplotlib as mpl
import numpy as np
import time
import math
from random import randint
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Line3DCollection
from matplotlib.colors import Normalize
import shm

x = shm.kalman.north
y = shm.kalman.east
z = shm.depth.depth
var = shm.dvl.temperature


raw_input("WARNING: Will reset absolute position to 0,0 for plotting purposes.\n"+
             "Ctrl+C to abort. Push enter to continue...")
x.set(0)
y.set(0)
print "Position reset to (0,0). Starting plot..."


max_samples = 50 
sample_interval = 0.1

mpl.interactive(True)
mpl.use('WXAgg')

data = []

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
        data = []
        self.tmin = 0
        self.tmax = 0

        PlotPanel.__init__(self, parent, *args, **kwargs)

    def make(self):
        self.axes = Axes3D(self.figure)

        x = np.linspace(0,1,50)
        y = x
        z = x
        t = x
        points = np.array([x,y,z]).T.reshape(-1,1,3)
        segments = np.concatenate([points[:-1], points[1:]], axis=1)
        norm = Normalize()
        self.lc = Line3DCollection(segments, norm=norm, cmap=mpl.cm.get_cmap("hot"))
        self.lc.norm = norm
        self.lc.set_array(t)

        self.axes.add_collection3d(self.lc)

        self.redraw_timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.draw, self.redraw_timer)
        self.redraw_timer.Start(1000*sample_interval)

    def draw(self, event=None):
        data.append((x.get(), y.get(), z.get(), var.get()))

        xDat = np.array([p[0] for p in data])
        yDat = np.array([p[1] for p in data])
        zDat = np.array([p[2] for p in data])
        tDat = np.array([p[3] for p in data])
        #tDat = np.linspace(0,1,len(xDat))
        points = np.array([xDat,yDat,zDat]).T.reshape(-1,1,3)
        segments = np.concatenate([points[:-1], points[1:]], axis=1)

        self.lc.set_segments(segments)
        self.lc.set_array(tDat)
        self.lc.norm.autoscale(tDat)
        self.lc.set_linewidth(2)

app = wx.PySimpleApp(False)

def Startup():
    global frame
    frame = wx.Frame(None, wx.ID_ANY, "Shm Plotter", size=(400,400))
            
    #Make the menu
    menubar = wx.MenuBar()
    frame.SetMenuBar(menubar)

    panel = Plot(frame)

    frame.Show(True)

Startup()
app.MainLoop()

out = file("temperature_log.csv", "w")
for datum in data:
    out.write(", ".join([str(var) for var in datum])+"\n")
out.close()
