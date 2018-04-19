#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# CUAUV Shared Memory Editor
# Benjamin Seidenberg
# Alex Spitzer TODO
# - bind shift-P popping to entire window not just treectrl
# - resizing issue with plot when window is maximized
# - there may be a strange bug with addition of new thrusters
#     found using the webgui's button "Zero Thrusters". shm-editor doesn't reflect changes.........


import wx, wx.gizmos, sys, os
import wx.lib.newevent
from threading import Thread
from copy import copy
from time import time, sleep
import os

import types
import inspect

import shm
import shm.base

PLOTTERFILENAME = "shm_plotter"
TEMPDATAFILE = "temp"
PLOTTERCMD = os.path.join(os.path.dirname(os.path.realpath(__file__)), "%s.py" \
                                                        % PLOTTERFILENAME)
plotter = __import__(PLOTTERFILENAME)

PLOTSTRING = " (*)"   # this is added to variables that are plotted, should be a string that is NEVER present in variable name
PLOTSTRING = ""  # flawed because variables can change within the plotter and the editor has no way of knowing what has changed

VarUpdateEvent, EVT_VAR_UPDATE = wx.lib.newevent.NewEvent()

class WatcherThread(Thread):
    def __init__(self, frame):
        Thread.__init__(self)
        self.frame = frame
        self.watcher = shm.watchers.watcher()
        self.last_cur_open = copy(frame.cur_open_vars)
        self.finished = False
        self.last_update = time()
    
    def run(self):
        while not self.finished:
            cur_open_vars = copy(self.frame.cur_open_vars)

            for group, group_vars in (cur_open_vars - self.last_cur_open):
                self.watcher.watch(group)

            for group, group_vars  in (self.last_cur_open - cur_open_vars):
                self.watcher.unwatch(group)
            
            self.last_cur_open = cur_open_vars

            #Only update when we need to
            self.watcher.wait()
             
            if self.finished:
                break
            
            now = time()

            # limit updates to 12.5 times a second
            if now - self.last_update < 0.08:
                continue
                
            self.last_update = time()
            wx.PostEvent(self.frame, VarUpdateEvent(updated=self.frame.cur_open_vars)) 

class MyFrame(wx.Frame):
    cur_prefix_list = []
    cur_prefix_nodes = []
    cur_open_vars = set()
        
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)
        self.panel_1 = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        self.tree_ctrl = wx.gizmos.TreeListCtrl(self.panel_1, -1, style=wx.TR_NO_LINES|wx.TR_FULL_ROW_HIGHLIGHT|
                                                                          wx.TR_DEFAULT_STYLE|
                                                                          wx.TR_HAS_BUTTONS|
                                                                          wx.TR_HIDE_ROOT|
                                                                          wx.TR_EDIT_LABELS)
        
        self.tree_ctrl.AddColumn("Variable", width=300, edit=False)
        self.tree_ctrl.AddColumn("Value", width=180, edit=True)

        self.plot = None

        self.__set_properties()
        self.__do_layout()

        self.SetPosition((50,50))	
        self.SetFocus()
        self.tree_ctrl.SetFocus()

        self.Bind(wx.EVT_TREE_ITEM_EXPANDED, self.tree_expand, self.tree_ctrl)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.tree_collapse, self.tree_ctrl)
        self.Bind(wx.EVT_TREE_BEGIN_LABEL_EDIT, self.begin_edit, self.tree_ctrl)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT, self.end_edit, self.tree_ctrl)
        self.Bind(EVT_VAR_UPDATE, self.update_vals)
        # screw wx
        self.tree_ctrl.GetMainWindow().Bind(wx.EVT_KEY_DOWN, self.key_down)

        data = wx.TreeItemData()
        data.SetData([])
        root = self.tree_ctrl.AddRoot("root", data=data)


        #Parse the list of variables by inspecting the module
        #Some things aren't groups or vars, but look like them!
        non_groups_in_shm = ["watchers"]

        #Go through all the things in the module that are groups
        groups = [(group, getattr(shm,group)) for group in shm.__all__ if group not in non_groups_in_shm]
        groups.sort() #python is fun
        for name, group in groups:
            data = wx.TreeItemData()
            group_item = self.tree_ctrl.AppendItem(parent=root,
                                                      text=name, data=data)
        
            #Try all the variables in the group to see if they're vars
            group_vars = []
            for var in dir(group):
                var_cand = getattr(group, var)
                if inspect.isclass(var_cand) and \
                   issubclass(var_cand, shm.base.ShmVar):
                    var_data = wx.TreeItemData()
                    var_data.SetData(var_cand)
                    group_vars.append(self.tree_ctrl.AppendItem(parent=group_item, text=var, data=var_data))
            
            #Add it to the GUI tree
            data.SetData( (group, tuple(group_vars)) )

        self.cur_prefix_nodes.append(self.tree_ctrl.GetRootItem())
        
        self.wt = None

    def __set_properties(self):
        self.SetTitle("Shared Memory Editor")

    def __do_layout(self):
        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer_2 = wx.BoxSizer(wx.VERTICAL)             # makes tree list expand

        self.plot_but = wx.Button(self.panel_1, label = "Plot")
        self.plot_but.Bind(wx.EVT_BUTTON, lambda x: self.remove_plot() if self.plot is not None else self.add_plot())
        sizer_2.Add(self.plot_but, 0, wx.EXPAND)

        sizer_2.Add(self.tree_ctrl, 1, wx.EXPAND, 0)
        self.panel_1.SetSizer(sizer_2)

        self.sizer.Add(self.panel_1, 1, wx.EXPAND, 0)

        self.SetSizer(self.sizer)

        self.sizer.Fit(self)
        self.sizer.SetSizeHints(self)
        self.Layout()

        self.SetClientSize(wx.Size(500, 600))
                    
    def tree_expand(self, event):
        data = self.tree_ctrl.GetItemPyData(event.GetItem())
        if isinstance(data, types.TupleType) :
            self.cur_open_vars.add( data )
        wx.PostEvent(self, VarUpdateEvent(updated=self.cur_open_vars)) 
        self.wt.watcher.broadcast() 
        event.Skip()
    
    def tree_collapse(self, event):
        data = self.tree_ctrl.GetItemPyData(event.GetItem())
        if isinstance(data, types.TupleType):
            self.cur_open_vars.remove( data )
        wx.PostEvent(self, VarUpdateEvent(updated=self.cur_open_vars)) 
        self.wt.watcher.broadcast() 
        event.Skip()
    
    def begin_edit(self, event):
        if self.tree_ctrl.GetChildrenCount(event.GetItem()) > 0:
            event.Veto()
    
    def end_edit(self, event):
        var = self.tree_ctrl.GetItemPyData(event.GetItem())
        try:
            var.set(eval(event.GetLabel()))
        except Exception, detail:
            try:
                var.set(event.GetLabel())
            except Exception, detail:
                print "Attempted to set ", var, " to invalid value: ", detail
        # veto it as the other thread should catch this and update it correctly below
        event.Veto()
    
    def key_down(self, event):
        eventid = event.GetKeyCode()
        if eventid == wx.WXK_RETURN:
            select = self.tree_ctrl.GetSelection()
            if self.tree_ctrl.GetChildrenCount(select) > 0:
                if self.tree_ctrl.IsExpanded(select):
                    self.tree_ctrl.Collapse(select)
                else:
                    self.tree_ctrl.Expand(select)
            else:
                self.tree_ctrl.EditLabel(select, column=1)

        elif eventid == wx.WXK_RIGHT:
            select = self.tree_ctrl.GetSelection()
            if self.tree_ctrl.GetChildrenCount(select) == 0:
                var = self.tree_ctrl.GetItemPyData(select)

                group_name = self.tree_ctrl.GetItemText(self.tree_ctrl.GetItemParent(select)) # create string to name the variable using its group
                var_name = self.tree_ctrl.GetItemText(select)
                self.plot_var(var, "%s.%s" % (group_name, var_name))
            else:
                event.Skip()

        elif eventid == ord("P") and event.ShiftDown():
            self.pop_plot()

        else:
            event.Skip()
    
    def update_vals(self, event):
        for group, group_vars in event.updated:
            for var in group_vars:
                val = str(self.tree_ctrl.GetItemPyData(var).get())
                self.tree_ctrl.SetItemText(var, val, column=1)

    def add_plot(self):
        self.plot_but.SetLabel("Kill Plot")
        self.plot = plotter.Main(self, [], [])

        self.savebutton = wx.Button(self, label = "Save", id=-1)
        self.savebutton.Bind(wx.EVT_BUTTON, self.save_group)

        self.popbutton = wx.Button(self, label = "Pop", id=-1)
        self.popbutton.Bind(wx.EVT_BUTTON, self.pop_plot)

        self.grouptext = wx.TextCtrl(self, id=-1)

        sizer_3 = wx.BoxSizer(wx.VERTICAL)                # pop button and textbox button on top of plot
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)              # pop button next to textbox

        sizer_4.Add(self.savebutton, 0)
        sizer_4.Add(self.grouptext, 1, wx.EXPAND)
        sizer_4.Add(self.popbutton, 0)

        sizer_3.Add(sizer_4, 0, wx.EXPAND)
        sizer_3.Add(self.plot, 1, wx.EXPAND, 10)

        self.sizer.Add(sizer_3, 1, wx.EXPAND)
        self.SetClientSize(wx.Size(1000, 600))

    def remove_plot(self):
     #   def iterchildren(treectrl, node):
     #       cid, citem = treectrl.GetFirstChild(node)
     #       while cid.IsOk():
     #           yield cid
     #           cid, citem = treectrl.GetNextChild(node, citem)

      #  for group in iterchildren(self.tree_ctrl, self.tree_ctrl.GetRootItem()):
      #      for item in iterchildren(self.tree_ctrl, group):
      #          text = self.tree_ctrl.GetItemText(item)
      #          if text.find(PLOTSTRING) != -1:
      #              self.tree_ctrl.SetItemText(item, text.split()[0])

        # the above code makes sure that when plotter is popped the indications of plotting are removed e.g. varx (p) -> varx

        self.plot_but.SetLabel("Plot")
        self.sizer.Hide(1)
        self.sizer.Remove(1)
        self.popbutton.Destroy()
        self.grouptext.Destroy()
        self.savebutton.Destroy()
        self.plot.Destroy()
        self.plot = None
        self.SetClientSize(wx.Size(500, 600))

    def plot_var(self, var, varname):
        if self.plot is None:                    # first variable to be plotted
            self.add_plot()

        select = self.tree_ctrl.GetSelection()
        vname = self.tree_ctrl.GetItemText(select)
        if not self.plot.addVar(var, varname):              # variable is already plotted or cannot be plotted, a bit dirty
            self.plot.removeVar(var)
       #     self.tree_ctrl.SetItemText(select, vname.split()[0])   
            if not self.plot.getNumOfVars():                 # variable is last to be removed... remove whole plotter
                self.remove_plot()
 
        else:     # variable successfully added
           # self.tree_ctrl.SetItemText(select, vname + PLOTSTRING) 
            pass

    def save_group(self, event = None):
        text = self.grouptext.GetValue()
        if not text:
            print("No group name given... Aborted")
            return

        if not self.plot.exportVars(text.replace(" ", "_")):
            print("Exporting failed") 

        else:
            self.plot.updatePreferences()

    def pop_plot(self, event = None):
        if self.plot:
            argslist = self.plot.getVarNames()
            self.plot.exportData(TEMPDATAFILE)              # saves data to be used by new plotter process
            argslist += ["-d", TEMPDATAFILE]

            self.remove_plot()
            pid = os.fork()
            if not pid:
                os.execv(PLOTTERCMD, ["%s.py" % PLOTTERFILENAME] + argslist)

class ShmEditor(wx.App):
    def OnInit(self):
        wx.InitAllImageHandlers()
        self.frame_1 = MyFrame(None, -1, "")
        self.SetTopWindow(self.frame_1)
        self.frame_1.Show()
        return 1

if __name__ == "__main__":
    shm_editor = ShmEditor(0)
    wt = WatcherThread(shm_editor.frame_1)
    wt.daemon = True
    wt.start()
    shm_editor.frame_1.wt = wt
    shm_editor.MainLoop()
    wt.finished = True
