#!/usr/bin/env python2
import numpy as np
from chaco.api import Plot, ArrayPlotData, DataRange1D
from chaco.tools.api import RangeSelection, RangeSelectionOverlay, PanTool, ZoomTool
from traits.api import Array, HasTraits, Instance, Str, List, Any
from traitsui.api import View, Item, VGroup, ListEditor, InstanceEditor
from enable.api import ComponentEditor, Component

variables = dict()

colors = ["blue", "red", "green", "yellow", "cyan", "magenta", "black"]

class DataPlot(Component):
    '''An individual data plot to be presented in the LogPlot'''
    t = Array(value=np.linspace(0,20,100))
    data = Instance(ArrayPlotData)

    plot = Instance(Plot)

    eqn = Str("t")

    def _data_default(self):
        return ArrayPlotData(t=self.t, y0=eval(self.eqn,variables))
    def _t_changed(self):
        self.plot.data.set_data("t", self.t)

    def _eqn_changed(self):
        try:
            y = eval(self.eqn, variables)
        except Exception as e:
            import sys
            exc_type, exc_value, exc_traceback = sys.exc_info()
            import traceback
            print traceback.print_exception(type(e), e, exc_traceback)
            pass
        else:
            #Start by hiding all the current plots
            #TODO: this doesn't really work yet
            for p in self.plot.plots.values():
                p.visible = False

            if isinstance(y, tuple):
                for i,d in enumerate(y):
                    #Type check the output
                    if isinstance(d, np.ndarray) and (d.dtype != np.object):
                        self.data.set_data("y%s"%i, d)

                        if "plot%s"%i not in self.plot.plots:
                            self.plot.plot(('t', 'y%s'%i), name = "plot%s"%i, color=colors[i])
                        else:
                            self.plot.plots['plot%s'%i].visible = True
            else:
                if isinstance(y, np.ndarray) and (y.dtype != np.object):
                    self.data.set_data("y0", y)
                    self.plot.plots['plot0'].visible=True

    def _plot_default(self):
        plot = Plot(self.data)
        plot.plot( ('t', 'y0'), color=colors[0] )
        plot.padding=20
        plot.padding_left = 40

        plot.tools.append(PanTool(plot))#, constrain=True, constrain_direction="y"))
        #TODO: zoomtool works on both axes, should only affect y
        plot.tools.append(ZoomTool(plot, tool_mode="range", constrain=True, constrain_direction="y"))
        return plot

    traits_view = View(
            Item('plot', editor=ComponentEditor(), height=200, show_label=False, padding=0),
            Item('eqn', style="simple"),
            width=700,
            height=200,
            )

class LogPlot(HasTraits):
    '''Top-level component in the GUI. Contains multiple DataPlots.'''
    data_plots = List(Instance(DataPlot))
    time_plot = Instance(Plot)
    selection = Any()

    data = Instance(ArrayPlotData)

    time = Array()
    y = Array()

    def _data_default(self):
        return ArrayPlotData(t=self.time, y=self.y)

    def _data_plots_default(self):
        dplot = DataPlot(t = self.time)
        dplot2 = DataPlot(t = self.time)
        return [dplot, dplot2]

    def _time_default(self):
        return np.linspace(0,10,100)

    def handle_selection_change(self):
        selection = self.selection.selection

        if selection is None:
            low = np.min(self.time)
            high = np.max(self.time)
        else:
            low, high = selection

        #Update ranges in data plots
        for dp in self.data_plots:
            dp.plot.index_range = DataRange1D(low=low, high=high)

    def _time_plot_default(self):
        plot = Plot(self.data)
        line_plot = plot.plot(('t', 'y'))[0]

        line_plot.active_tool = RangeSelection(line_plot, left_button_selects=True)
        line_plot.overlays.append(RangeSelectionOverlay(component=line_plot))
        self.selection = line_plot.active_tool

        plot.padding = 20
        plot.padding_left = 50

        self.selection.on_trait_change(self.handle_selection_change, 'selection')

        return plot

    view = View(
            VGroup(
                Item('time_plot', editor=ComponentEditor(), show_label=False,
                        height=100, width=800, resizable=False, padding=0),
                Item('data_plots', editor=ListEditor(editor=InstanceEditor(), style='custom'), show_label=False, style='custom', padding=0),
                padding=0
                ),
            width=800,
            height=500,
            resizable=True,
            title="Log Plotter"
            )
        

#### Shared variable handling:

import shm
# Dictionary of the values of shared variables
shared_vars = dict()

class DummyShm(object):
    '''This class pretends to be a shared variable group
     and will load in any variables only when they are accesssed.
     This is to cut down on the large amount of memory and time it takes
     to store an entire log in memory at once in the format that we need'''

    def __init__(self, group_name):
        self.group_name = group_name
        self.var = shm.__dict__[group_name]

    def __getattr__(self, name):
        ''' uses the value from the shared_vars global.
        If name not in shared_vars, we need to reload data from
        the log file and update shared_vars '''
        global shared_vars, variables

        var_name = "%s.%s" % (self.group_name, name)
        try:
            return shared_vars[var_name]
        except KeyError:
            # Check to make sure this is a real var
            if name not in dir(self.var):
                return

            # Load the values for it
            print "adding %s" % var_name
            keys = shared_vars.keys() + [var_name]
            times, values = log_to_array(filename, 
                                         keys)
            shared_vars.update( dict(zip(keys, values)) )
            variables["t"] = times
            print [x.shape for x in shared_vars.values()]
            return shared_vars[var_name]


#### Actually running the program
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description="Plots shmlog files")
    parser.add_argument("filename", help="the shmlog file to load")
    parser.add_argument("--interactive", help="use if you want to run from 'ipython --pylab'", 
                        dest="interactive", const=True, default=False, 
                        action="store_const")
    args = parser.parse_args()
    filename = args.filename

    # Create dummy shared variables for all shared variables
    all_shared_vars_names = shm.__all__
    all_shared_vars_names.remove("watchers") # Remove non-watchers
    dummy_shared_vars = {name: DummyShm(name) for name in all_shared_vars_names}


    from logtoarray import log_to_array
    # Get some values to start with, but not really important what
    times, values = log_to_array(filename, ["sparton.accelx"])
    

    # Pull variables into the 'variables' scope for evaluation
    exec "from numpy import *" in variables
    import utils
    variables.update( utils.make_funcs(variables) )
    import kalman
    variables.update( kalman.make_funcs(variables) )

    # Add the actual data to the variables environment
    variables["t"] = times
    variables.update(dummy_shared_vars)

    # Run the plotter
    lp = LogPlot(time=times, y=values[0])
    if args.interactive:
        lp.edit_traits() # Non-blocking GUI
    else:
        lp.configure_traits() # Blocking GUI
