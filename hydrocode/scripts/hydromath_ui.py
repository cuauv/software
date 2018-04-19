#!/usr/bin/env python2
import wx
import wx.lib.mixins.inspection as wit

if 'phoenix' in wx.PlatformInfo:
    import wx.lib.agw.aui as aui
else:
    import wx.aui as aui

import matplotlib as mpl
mpl.interactive(True)
mpl.use('WXAgg')
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as Canvas
from matplotlib.backends.backend_wxagg import NavigationToolbar2Wx as Toolbar

import numpy as np

import matplotlib.pyplot as plt 
import shm
N = 100
class MainWindowFrame(wx.Frame):
    """ The main frame of the application """
    title = 'Hydromath UI'

    def __init__(self):
        wx.Frame.__init__(self,None,-1,self.title)
        plt.ion()
        self.create_main_panel()
        self.redraw_timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER,self.on_redraw_timer,self.redraw_timer)
        self.redraw_timer.Start(100)
    def create_main_panel(self):
        self.panel = wx.Panel(self)

        self.init_plot()

        self.canvas = Canvas(self.panel,-1,self.fig)
        self.vbox = wx.BoxSizer(wx.VERTICAL)
        self.vbox.Add(self.canvas,1,flag=wx.LEFT | wx.TOP | wx.GROW)

        self.panel.SetSizer(self.vbox)
        self.vbox.Fit(self)
    def init_plot(self):
        self.fig  = plt.Figure((3.0,3.0))
        self.axes = self.fig.add_subplot(111,polar=True)
        self.axes.set_theta_zero_location('N')
        self.axes.set_title('Ping Heading Polar Plot') 
        self.axes.set_rmax(1.0)
        self.axes.grid(True)
        self.theta=np.zeros((N,1))
        self.rho = np.linspace(0,1,N)
        self.heading_line, = self.axes.plot(self.theta,self.rho,color='r')
    def redraw_plot(self,ping_angle=0):
        theta = ping_angle*np.ones((N,1))
        self.heading_line.set_xdata(theta)
        self.canvas.draw()
    def on_redraw_timer(self,event):
        ping_angle = shm.hydrophones_results_track.tracked_ping_heading_radians.get()
        self.redraw_plot(ping_angle)
    def on_exit(self,event):
        self.Destroy()
if __name__ == '__main__':
    app = wx.PySimpleApp()
    app.frame = MainWindowFrame()
    app.frame.Show()
    app.MainLoop()
