#!/usr/bin/env python2
import wx
from auval.LED import *

noResize_frameStyle = wx.SYSTEM_MENU | wx.CLOSE_BOX | wx.CAPTION

class Frame(wx.Frame):
    def __init__(self, title):
        wx.Frame.__init__(self, None, title=title, size=(200,50),  style=noResize_frameStyle)

        panel = wx.Panel(self)

        hbox = wx.BoxSizer(wx.VERTICAL)

        fgs = wx.FlexGridSizer(1, 3, 9, 25)

        self.red = wx.StaticText(panel, label="RED")
        self.blue = wx.StaticText(panel, label="BLUE")
        self.green = wx.StaticText(panel, label="GREEN")

        fgs.AddMany([(self.red), (self.blue), (self.green)])

        self.red.SetForegroundColour((0,0,0))
        self.blue.SetForegroundColour((0,0,0))
        self.green.SetForegroundColour((0,0,0))
        
        fgs.AddGrowableRow(2, 1)
        fgs.AddGrowableCol(1, 1)

        hbox.Add(fgs, proportion=1, flag=wx.ALL|wx.EXPAND, border=15)
        panel.SetSizer(hbox)

        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.update, self.timer)
        self.timer.Start(10)

    def update(self, event):
        if LEDS['red'].get():
            self.red.SetForegroundColour((255,0,0))
        else:
            self.red.SetForegroundColour((0,0,0))
        if LEDS['green'].get():
            self.green.SetForegroundColour((0,255,0))
        else:
            self.green.SetForegroundColour((0,0,0))
        if LEDS['blue'].get():
            self.blue.SetForegroundColour((0,0,255))
        else:
            self.blue.SetForegroundColour((0,0,0))




app = wx.App(redirect=False)
top = Frame("LED Status")
top.Show()
app.MainLoop()
