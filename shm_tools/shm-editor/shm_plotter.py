#!/usr/bin/env python2
'''Uses wxPython and matplotlib to plot in real-time shared memory values'''
# TODO ALEX SPITZER
# - dragging the window is bugged when quickly clicking...
# - mouse over data points?
# - mousewheel scrolling!
# - minimum y-axis zoom: user changes in real time
# - reduce CPU usage!
# - cpu usage is ridiculous (constant load)

# INCREASE DRAW_INTERVAL TO IMPROVE PERFORMANCE

import wx
import matplotlib as mpl
import numpy as np
import time
import math
import os
from random import randint
import pickle
import shm

from matplotlib.font_manager import FontProperties

sample_interval = 1/100.0  # how many seconds between data retrieval i.e. 0.1 = 10 samples / s
draw_interval =   1/20.0  # draw interval; the lower the smoother, essentially FPS

MAX_HISTORY = int(15 * 60 / sample_interval)   # how many data points to keep in history

XSCROLLVEL = 0.4        # float from 0 to 1 configuring smoothscrolling; 0 is no motion, 1 is instantaneous
YSCROLLVEL = 0.5

DIRECTORY = os.path.dirname(os.path.realpath(__file__))
PREFFILENAME = os.path.join(DIRECTORY, "variablegroups")

colors = "rbgycm"
markers = ["-", "--"]
styles = [y+x for x in markers for y in colors]

mpl.interactive(True)
mpl.use('WXAgg')

class SmoothBound():
    """ A value that changes smoothly, call tick() to progress """
    def __init__(self, value, vel=0.25):
        self.value = value
        self.desire = value
        self.vel = vel

        # mostly to reduce CPU usage
        self.ratefactor = draw_interval / 0.1 # keep speed constant
        self.snapThresh = 0.04 * self.ratefactor
        self.veff = self.vel * self.ratefactor

    def setDesire(self, value):
        self.desire = value

    def shift(self, delta):
        self.setDesire(self.desire + delta)

    def jump(self, delta):
        self.value += delta
        self.desire += delta

    def tick(self):
        speed = (self.desire - self.value) * self.veff
        
        if abs(speed) < self.snapThresh:            # snap into place
            self.value = self.desire
        else:
            self.value += speed

class AxisBound():
    """ Two smooth values that represent a bound, call tick() to progress """     # could be replaced with a list of SmoothBound objects
    def __init__(self, low, high, vel = 0.25):
        self.low = SmoothBound(low, vel)
        self.high = SmoothBound(high, vel)

    def setHigh(self, high):
        self.high.setDesire(high)

    def setLow(self, low):
        self.low.setDesire(low)

    def zoom(self, zoom):
        self.setHigh(self.high.desire + zoom * 0.5)
        self.setLow(self.low.desire - zoom * 0.5)

    def lowZoom(self, zoom):
        self.setLow(self.low.desire - zoom)

    def shift(self, delta):
        self.setHigh(self.high.desire + delta)
        self.setLow(self.low.desire + delta)

    def tick(self):
        self.low.tick()
        self.high.tick()

    def getLow(self):
        return self.low.value

    def getHigh(self):
        return self.high.value

    def getLowD(self):
        return self.low.desire

    def getHighD(self):
        return self.high.desire


class PlotPanel( wx.Panel ):
    def __init__(self, parent, dpi = None, color=None, *args, **kwargs):
        from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg
        wx.Panel.__init__(self, parent, wx.ID_ANY, *args, **kwargs)
        self.parent = parent
        self.figure = mpl.figure.Figure( None, dpi )
        self.canvas = FigureCanvasWxAgg(self, -1, self.figure)

        self._resizeflag = False

        self._SetSize()

        self.Bind(wx.EVT_IDLE, self._onIdle)
        self.Bind(wx.EVT_SIZE, self._SetSize)

    def _SetSize(self, event=None):
        pixels = self.GetSize()
        self.SetSize(pixels)
        self.canvas.SetSize(pixels)
        self.figure.set_size_inches( float(pixels[0])/self.figure.get_dpi(),
                                     float(pixels[1])/self.figure.get_dpi() )

    def _onIdle(self,event):
        self.canvas.draw()
        if self._resizeflag:
            self._SetSize()
            self._resizeflag = False

    def draw(self):
        pass # To be overriden by children
    def make(self):
        pass # To be overriden by children
           

class Plotter(PlotPanel):
    def __init__(self, parent, variables, varnames, data = None, *args, **kwargs):
        PlotPanel.__init__(self, parent, *args, **kwargs)
        self.parent = parent
        self.variables = []     # variable objects, must have 'get' method
        self.plotdata = []      # matplotlib "lines"
        self.data = []          # lists of data for each variable 
        self.rBounds = []       # bounds of rightmost datapoint being displayed; one for each variable
        self.varnames = []      # display name for each variable
        self.visible = []       # list of bools to toggle variables
        self.offsets = []       # list of offsets for each variable
        self._ylock = 0
 
        self.make()

        self._zoomRate = int(1/sample_interval)
        self.N = SmoothBound(5 / sample_interval, XSCROLLVEL)     # number of data points on plot
        self.updateSeekRate()

        self.xbound = AxisBound(-self.N.desire * sample_interval, 0, XSCROLLVEL)
        self.ybound = AxisBound(0, 0, YSCROLLVEL)
      
        for (i, var) in enumerate(variables):
            if data:                                # variables come with data
                self.addVar(var, varnames[i], data[i])
            else:
                self.addVar(var, varnames[i], [])

#        self.Rescale()

    def make(self):  
        self.live = 1

        self.IOtimer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.update, self.IOtimer)
        self.IOtimer.Start(1000*sample_interval)

        self.DrawTimer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.draw, self.DrawTimer)
        self.DrawTimer.Start(1000*draw_interval)

        self.canvas.Bind(wx.EVT_KEY_DOWN, self.onChar)

        self.axes = self.figure.add_subplot(111)
        self.axes.set_xlabel("Seconds")
        self.axes.set_title("LIVE")
        self.axes.yaxis.grid(color='gray', linestyle='dashed')
        self.axes.xaxis.grid(color='gray', linestyle='dashed')

        self.legendfont = FontProperties(size='medium')

    def makeLegend(self):
        self.legend = self.axes.legend(loc="upper left", prop=self.legendfont)
 
    def Rescale(self, event = None):              # currently not used, y-axis is always auto scaled
        try:
            self.ybound.setLow(min([min(datas) for datas in self.data]))
            self.ybound.setHigh(max([max(datas) for datas in self.data]))
        except ValueError:         # no variables plotted
            self.ybound.setLow(0)
            self.ybound.setHigh(0)

    def updateSeekRate(self):                     # the more zoomed out, the more you seek
        self._seekRate = int(self.N.desire * 0.25)
        if not self._seekRate: self._seekRate = 1

    def addVar(self, var, varname, data = None):
        if var in self.variables:
            return 0

        if type(var.get()) not in [int, float]:
            print("Attempting to add a non float or int value... Aborted")
            return 0

        if data is None: data = []    # date must be None in function header line because otherwise mutable lists cause funky problems...

        self.rBounds.append(SmoothBound(len(data), XSCROLLVEL))
        self.variables.append(var)
        self.data.append(data)
        self.varnames.append(varname)
        self.visible.append(True)
        self.offsets.append(0)
        newline = mpl.lines.Line2D([], [])
     
        # get an unused style
        usedstyles = set([line.get_c() + line.get_ls() for line in self.plotdata])
        style = None
        for s in styles:
            if s not in usedstyles:
                style = s
                break
        else:
            print("Using duplicate style... You've plotted too many variables")
            style = styles[randint(0, len(styles)-1)]

        newline.set_c(style[0])
        newline.set_ls(style[1:])

        newline.set_label(varname)
        self.plotdata.append(self.axes.add_line(newline))
        self.makeLegend()
        return 1

    def removeVar(self, var):
        if var not in self.variables:
           # print("Attempting to remove variable that is not present...Aborted")
            return 0 
 
        i = self.variables.index(var)   # assumes all appropriate storages are ordered...
        self.rBounds.pop(i)
        self.data.pop(i)
        line = self.plotdata.pop(i)
        self.axes.lines.remove(line)

        self.variables.pop(i)
        name = self.varnames.pop(i)
        self.visible.pop(i)
        self.offsets.pop(i)

        if len(self.variables): # only make legend if more variables remain
            self.makeLegend()

        return 1


    def onChar(self, event):
        eventid = event.GetKeyCode()
        if not self.live:
            if eventid == wx.WXK_LEFT:
                [r.shift(-self._seekRate) for r in self.rBounds]
                self.xbound.shift(-self._seekRate * sample_interval)

            elif eventid == wx.WXK_RIGHT:
                [r.shift(self._seekRate) for r in self.rBounds]
                self.xbound.shift(self._seekRate * sample_interval)

        if eventid == wx.WXK_UP and self.N.desire > self._zoomRate:   # zoom in x-axis
            self.N.shift(-self._zoomRate)
            self.updateSeekRate()
            self.xbound.lowZoom(-self._zoomRate * sample_interval)

        elif eventid == wx.WXK_DOWN:      # zoom out x-axis
            self.N.shift(self._zoomRate)
            self.updateSeekRate()
            self.xbound.lowZoom(self._zoomRate * sample_interval)

        elif eventid == wx.WXK_SPACE:     # toggle plotter pause
            self.onSpace(event)

        elif eventid == ord("L") and len(self.variables):   # toggle the legend
            self.legend.set_visible(1 - self.legend.get_visible())

        elif eventid == ord("G"):         # toggle the gridlines
            self.axes.grid()

        # Newer versions of wx seem to move the focus to another widget
        # after arrow key presses. This ensures that it returns to the canvas
        # after the key press so that more keys can be pressed and processed.
        wx.CallAfter(self.canvas.SetFocus)

        event.Skip()

    def onSpace(self, event=None):
        if self.live:
            self.live = 0
            self.pauseline = self.axes.axvline(x = 0)
            self.axes.set_title("PAUSED")
        else:
            self.axes.lines.remove(self.pauseline)
            self.axes.set_title("LIVE")
            self.live = 1
            self.xbound.setHigh(0)
            self.xbound.setLow(-self.N.desire * sample_interval)
            [self.rBounds[i].setDesire(len(self.data[i])) for i in range(len(self.data))]

    def clearData(self, event = None):     # erase the data history
        self.data = [[] for i in range(len(self.variables))]
        self.rBounds = [SmoothBound(0) for i in range(len(self.variables))]
        self.xbound = AxisBound(-self.N.desire * sample_interval, 0)

    def toggleLockZoom(self, event = None):
        self._ylock = 1 - self._ylock

    def toggleVar(self, varname):
        try:
            i = self.varnames.index(varname)
        except ValueError:
            print "Attempting to toggle variable that doesn't exist... Aborted"
            return 0

        self.visible[i] = 1 - self.visible[i]
        return 1

    def setOffset(self, var_ind, offset):
        self.offsets[var_ind] = offset
        to_add = ""
        if offset:
            sign = "+" if offset > 0 else "-"
            to_add += " (%s %.2f)" % (sign, abs(offset))

        self.plotdata[var_ind].set_label(self.varnames[var_ind] + to_add)
        self.makeLegend()

    def getOffset(self, var_ind):
        return self.offsets[var_ind]

    def update(self, event = None):
        for i in range(len(self.variables)):
            self.data[i].append(self.variables[i].get())

            if self.live:
                if len(self.data[i]) > MAX_HISTORY:
                    [(self.data[i].pop(0), self.rBounds[i].jump(-1)) for k in range(len(self.data[i]) - MAX_HISTORY)]
         
                self.rBounds[i].jump(1)

    def draw(self, event = None):
        self.xbound.tick()
        self.ybound.tick()
        self.N.tick()
        [r.tick() for r in self.rBounds]

        ymax, ymin = None, None
        for i in range(len(self.variables)):
            if not self.visible[i]:    # variable has been toggled off
                self.plotdata[i].set_data([], [])
                continue

            bound = self.rBounds[i].value
            if bound < 1 or bound - self.N.value > len(self.data[i]):         # don't plot this variable if there is no data for it in the timeframe
                continue

            ydata = self.data[i][(int(bound) - (int(self.N.value) + 1))*(bound >= self.N.value + 1):int(bound)]
            xdata = [sample_interval*(x - (bound - len(self.data[i])) * (bound > len(self.data[i]))) + self.xbound.getHigh() for x in range(-len(ydata)+1, 1)]

            # add offset to variable
            if self.offsets[i]:
                ydata = [y + self.offsets[i] for y in ydata]

            self.plotdata[i].set_data(xdata, ydata) 

            if ymin is not None:   
                ymax = max(ymax, max(ydata))
                ymin = min(ymin, min(ydata))
            else:
                ymax = max(ydata)
                ymin = min(ydata)

        if ymin is not None:
            if self.ybound.getLowD() > ymin or not self._ylock:
                self.ybound.setLow(ymin)
            if self.ybound.getHighD() < ymax or not self._ylock:
                self.ybound.setHigh(ymax)

        ymax = self.ybound.getHigh()
        ymin = self.ybound.getLow() 
        diff = ymax - ymin
        if not diff: diff = 10   

        self.axes.set_ybound(lower = ymin - abs(diff * 0.1) , upper = ymax + abs(diff * 0.1))  # add 10% padding so line can be seen

        xmin = self.xbound.getLow()
        xmax = self.xbound.getHigh()        
        self.axes.set_xbound(lower=xmin, upper=xmax)


class Main(wx.Panel):
    def __init__(self, parent, variables, varnames, data = None, preffilename = PREFFILENAME, *args, **kwargs):
        wx.Panel.__init__(self, parent, wx.ID_ANY, *args, **kwargs)

        self.plot = Plotter(self, variables, varnames, data, *args, **kwargs)
        self.makeMenu()

        # Make the layout
        # One vertical sizer for top panel and bottom panel (plot) 
        # One horizontal sizer for text and combobox
 
        self.sizer = wx.BoxSizer(wx.VERTICAL)

        self.topsizer = wx.BoxSizer(wx.HORIZONTAL)

        text = wx.StaticText(self, -1, "Plot group:")
        self.topsizer.Add(text, 0, wx.ALIGN_CENTER | wx.LEFT | wx.RIGHT, 10)

        self.preffilename = preffilename
        self.pulldown = wx.ComboBox(self, style = wx.CB_READONLY)
        self.updatePreferences()

        self.topsizer.Add(self.pulldown, 0)

        self.clearButton = wx.Button(self, label = "Clear", size = (60, 30))
        self.clearButton.Bind(wx.EVT_BUTTON, self.plot.clearData)

        self.topsizer.Add(self.clearButton, 0)

        self.zoomToggle = wx.CheckBox(self, label = "Y-axis min zoom:", style = wx.ALIGN_RIGHT)
        self.zoomToggle.Bind(wx.EVT_CHECKBOX, self.plot.toggleLockZoom)

        self.topsizer.Add(self.zoomToggle, 0)

        self.sizer.Add(self.topsizer, 0)

        self.sizer.Add(self.plot, 1, wx.EXPAND)

        self.SetSizer(self.sizer)
        self.sizer.Layout()

        self.pulldown.Bind(wx.EVT_COMBOBOX, self.onComboSelect)

    def updatePreferences(self):
        try:
            with open(self.preffilename, "r") as f:
                self.preferences = f.readlines()
        except IOError:
            print("No preference file found.")
            self.preferences = []
 
        groups = [line.split()[0] for line in self.preferences]
        self.pulldown.Clear()
        [self.pulldown.Append(group) for group in groups]

    def addVar(self, var, varname, data = None):
        status = self.plot.addVar(var, varname, data)
        if status:
            self.makeMenu()

        return status

    def removeVar(self, var):
        status = self.plot.removeVar(var)
        if status:
            self.makeMenu()

        return status

    def onComboSelect(self, event):
        group = self.pulldown.GetValue()
        newvariables = getvariables(self.preferences, group)
        if not newvariables:
            print("Error: group not found... Aborted")
            return 0
             
        for var in self.plot.variables[:]:
            self.removeVar(var)

        for var in newvariables:
            self.addVar(shm._eval(var), var)

        self.plot.SetFocus()    # ready to analyze

    def makeMenu(self):
        menubar = wx.MenuBar()
        frame = self.GetTopLevelParent()
        frame.SetMenuBar(menubar)
 
        variableMenu = wx.Menu()
        offsetMenu = wx.Menu()
        for (i, var) in enumerate(self.getVarNames()):
            but = variableMenu.Append(wx.ID_ANY, var, var, kind=wx.ITEM_CHECK)
            frame.Bind(wx.EVT_MENU, \
                       lambda x, var=var: self.plot.toggleVar(var), but)

            if self.plot.visible[i]:
                but.Toggle()

            but = offsetMenu.Append(wx.ID_ANY, var + "...", var, \
                                    kind=wx.ITEM_NORMAL)
            frame.Bind(wx.EVT_MENU, \
                       lambda x, i=i, var=var: self.askOffset(i, var), but)

        n_vars = len(self.getVarNames())
        if n_vars:
            tit = "Clear All Offsets"
            but = offsetMenu.Append(wx.ID_ANY, tit, tit, kind=wx.ITEM_NORMAL)
            frame.Bind(wx.EVT_MENU, \
              lambda x: [self.plot.setOffset(i, 0) for i in range(n_vars)], but)

        menubar.Append(variableMenu, "Variables")
        menubar.Append(offsetMenu, "Offsets")

    def askOffset(self, var_ind, var_name):
        dialog = wx.TextEntryDialog(None, "Enter new offset for %s:" %var_name,\
                    "Set Offset", str(self.plot.getOffset(var_ind)),style=wx.OK)

        call_back = lambda x, vi=var_ind, dg=dialog: self.offsetCallback(vi, dg)
        dialog.Bind(wx.EVT_BUTTON, call_back)
        dialog.Bind(wx.EVT_CLOSE, lambda x, dg=dialog: dg.Destroy())
        dialog.Show()

    def offsetCallback(self, var_ind, dialog):
        try:
            new_offset = float(dialog.GetChildren()[1].GetValue())
        except ValueError:
            print "Invalid offset"
        else:
            self.plot.setOffset(var_ind, new_offset)

        dialog.Destroy()

    def getNumOfVars(self):
        return len(self.plot.variables)

    def getVarNames(self):
        return self.plot.varnames

    def exportVars(self, groupname = "Custom"):
        """ Writes varaibles plotted as a group to a pref file """
        if not len(self.plot.variables):
            print("Attempting to export with no variables... Aborted")
            return 0

        if not self.preferences:
            print("Creating new preference file %s" % self.preffilename)

        used = [line.split()[0] for line in self.preferences]
        i = 2
        newgroupname = groupname
        while newgroupname in used:
            newgroupname = "%s_%d" % (groupname, i)
            i += 1

        with open(self.preffilename, 'a') as f:  # this assumes variable name is actual variable!!!!
            f.write(newgroupname)
            [f.write(" %s" % varname) for varname in self.plot.varnames]
            f.write(os.linesep)

        self.updatePreferences()
        return newgroupname

    def exportData(self, outfilename="temp"):
        """ Pickles data in a temp file for use by a "popped" plotter """
        with open(os.path.join(DIRECTORY, outfilename), "w") as f:
           p = pickle.Pickler(f)
           p.dump(self.plot.data)
            

def getvariables(prefs, group):
    ''' takes a list of lines from a pref file and returns a list of variable objects or an empty list if the group was not found'''
    for line in prefs:
        words = line.split()
        if words[0] == group:
            return words[1:]
    return None
 



if __name__ == "__main__":
    app = wx.PySimpleApp(False)

    import argparse
    parser = argparse.ArgumentParser(description="Shm Plotter")
    parser.add_argument('-g', dest="group", action="store_const", const=True, default=False, help= "add this flag to name a group from the preffile")
    parser.add_argument('-d', dest='data', nargs = 1, default = False, help = "add this flag to start with data saved in temp file")
    parser.add_argument('variables', nargs='*', default=[], help="variables to plot")

    args = parser.parse_args()
    if args.group:
        if not len(args.variables):
            print "No group given"
            quit()

        with open(PREFFILENAME, "r") as f:
            preferences = f.readlines()

        varnames = getvariables(preferences, args.variables[0]) 

    else:
        varnames = args.variables[:]
 
    if varnames is None:   # getvariables returned None because group was not found
        print "Invalid group %s" % args.variables[0]
        quit()
  
    variables = []   
    for varname in varnames[:]:
        try:
            var = shm._eval(varname)

        except shm.ShmEvalError:
            print "Invalid shm variable %s" % varname
            varnames.remove(varname)

        else:
            variables.append(var)

    print "Plotting %s" % ' '.join(varnames)

    if args.data:        # pick up data left by popped plotter
        filename = os.path.join(DIRECTORY, args.data[0])
        with open(os.path.join(filename), "r") as f:
            p = pickle.Unpickler(f)
            data = p.load()
        os.remove(filename)
    else:
        data = []

    frame = wx.Frame(None, wx.ID_ANY, "Shm Plotter 2.1", size=(600, 400))
    plot = Main(frame, variables, varnames, data, PREFFILENAME)

    frame.Show(True)
    app.MainLoop()
