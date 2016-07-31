#!/usr/bin/env python2

import wx
import wx.lib.dragscroller
import shm
import pickle
import math
scale = 10
xoff = (75/2.0)*scale
yoff = (75/2.0)*scale
xpad = 0
ypad = 0
xpoints = [0]
ypoints = [0]
currentlayout = 0
panel = None
followSub = False
    
#-------------------------------------------------------------------------------
class legend(wx.Frame):
    def __init__(self, parent):
        wx.Frame.__init__(self,parent,size=(165,185))
        self.Bind(wx.EVT_PAINT, self.OnPaint)

    
    def OnPaint(self, event):
        xloc = 5
        shapeLocations = []
        labelLocations = []

        dc = wx.PaintDC(self)
        dc.SetTextForeground(wx.BLACK)
        labels = ["Start", "", "Orange buoy", "Yellow buoy", "Green buoy", "Wire", "Pipe to emperor", "Pipe to bins", "Torpedoes", "Pipe bins to emperor", "Pipe to wire", "Emperor"]
        
        for i in range(0, 12):
            shapeLocations.append([(5 + i*15), xloc])
            labelLocations.append([xloc + 15, (5 + i*15)-3])

        dc.DrawTextList(labels, labelLocations)

        for p in range(0, len(shapeLocations)):
            DrawShapes(self, shapeLocations[p], p)


class DragScrollerExample(wx.ScrolledWindow):
    def __init__(self, parent, id=-1):
        wx.ScrolledWindow.__init__(self, parent, id)
        self.timer = wx.Timer(self)  
        global xoff
        global yoff
        global xpad
        global ypad
        global panel
        xpoints[0] = shm.kalman.north.get()
        ypoints[0] = shm.kalman.east.get()
        xoff = scale * ((75/2.0) - xpoints[0] + xpad)
        yoff = scale * ((75/2.0) - ypoints[0] + ypad)

        #menu bar, making window visible, etc
        frame.CreateStatusBar()
        menuBar = wx.MenuBar()
        menu = wx.Menu()
        menuBar.Append(menu, "&File")
        frame.SetMenuBar(menuBar)
        frame.Show(True)
        panel = wx.Panel(self, size = (105,105))

        self.button1 = wx.Button(panel, id=-1, label="Legend", pos=(2,2), size=(90, 30))
        self.button2 = wx.Button(panel, id=-1, label="Reset", pos=(2,37), size=(80, 30))
        self.button3 = wx.Button(panel, id=-1, label="Follow Sub", pos=(2,72), size=(100, 30))
        
        # Events.
        self.Bind(wx.EVT_PAINT, self.OnPaint)
       
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.button1.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.button2.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        self.button3.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
        #needs to be binded for each button because can't tell which has focus
        
        self.button1.Bind(wx.EVT_BUTTON, self.button1Click)
        self.button2.Bind(wx.EVT_BUTTON, self.button2Click)
        self.button3.Bind(wx.EVT_BUTTON, self.button3Click)
        
        self.Bind(wx.EVT_KILL_FOCUS, self.setFocus)
        self.Bind(wx.EVT_TIMER, self.OnTimer, self.timer)
        
        self.timer.Start(100) 
    

    def setFocus(self, event):
        self.SetFocus()


    def button1Click(self, event):
        #show legend
        global panel
        if (self.button1.GetLabel() == "Legend"):
            self.button1.SetLabel("Hide Legend")
            panel = wx.Frame(None)
            panel = legend(panel)
            panel.Show()
        else:
            self.button1.SetLabel("Legend")
            try:
                panel.Destroy()
            except:
                pass


    def button2Click(self, event):
        #clear all points
        global xpoints
        global ypoints
        xpoints = [0]
        ypoints = [0]
        self.OnPaint(1)
    
    
    def button3Click(self, event):
        global followSub
        if (followSub == False):
            self.button3.SetLabel("Unfollow Sub")
            followSub = True
            xpad = 75/2.0 - xpoints[len(xpoints) - 1]
            ypad = 75/2.0 - ypoints[len(ypoints) - 1]
        else:
            self.button3.SetLabel("Follow Sub")
            followSub = False



    def OnKeyDown(self,e):
        global scale
        global xpoints
        global ypoints
        global xoff
        global yoff
        global xpad
        global ypad
        #left 314/up 315/right 316/down 317
        if e.GetKeyCode() == 61: #+ key
            scale = scale + 5
            self.setXYOff("zoom")
        elif e.GetKeyCode() == 45: #- key
            if scale > 5:
                scale = scale - 5
                self.setXYOff("zoom")
        elif e.GetKeyCode() == 315: #up arrow key
            ypad = ypad + 5
            self.setXYOff("shift")
        elif e.GetKeyCode() == 317: #down arrow key
            ypad = ypad - 5
            self.setXYOff("shift")
        elif e.GetKeyCode() == 314: #left arrow key
            xpad = xpad + 5
            self.setXYOff("shift")
        elif e.GetKeyCode() == 316: #right arrow key
            xpad = xpad - 5
            self.setXYOff("shift")
        self.SetScrollbars(1, 1, 75*scale, 75*scale, 0, 0)
        self.OnPaint(1)
        print "xoff"
        print xoff
        print "yoff"
        print yoff


    def OnTimer(self, evt):
        global currentlayout  
        global followSub
        global xpad
        global ypad
             
        dc = wx.PaintDC(self)
        self.DoPrepareDC(dc) 
        self.SetFocus()
        pen = wx.Pen((178,34,34), 2)
        brush = wx.Brush((255,255,255))
        dc.SetBrush(brush)
        dc.SetPen(pen)
        self.OnPaint(1)
        if len(xpoints) == 0:
            xpoints.append(-shm.kalman.north.get())
            ypoints.append(shm.kalman.east.get())
        elif xpoints[len(xpoints)-1] != shm.kalman.north.get() or ypoints[len(ypoints)-1] != shm.kalman.east.get():
            xpoints.append(-shm.kalman.east.get())
            ypoints.append(shm.kalman.north.get())
            if (followSub and len(xpoints) > 1):
                xpad = xpad - (xpoints[len(xpoints)-1] - xpoints[len(xpoints)-2])
                ypad = ypad - (ypoints[len(ypoints)-1] - ypoints[len(ypoints)-2])
        for i in range(0,len(xpoints)):
            pointLoc = worldToScreen(xpoints[i],ypoints[i],"drawPath")
            dc.DrawLine(pointLoc[0], pointLoc[1], pointLoc[0], pointLoc[1])  #cant use drawpoint because you cant adjust the pen size


        ship_polygon = [(0,10), (5,-5), (-5,-5)]
        angle = math.radians(shm.kalman.heading.get())
        ship_polygon = [wx.Point(int(point[0]*math.cos(angle) - point[1]*math.sin(angle)), int(point[0]*math.sin(angle) + point[1]*math.cos(angle))) for point in ship_polygon]
        pen = wx.Pen(wx.RED, 1)

        brush = wx.Brush(wx.RED) 
        dc.SetPen(pen)
        dc.SetBrush(brush)
        shipLoc = worldToScreen(xpoints[len(xpoints)-1],ypoints[len(ypoints)-1],"drawSub")
        dc.DrawPolygon(ship_polygon, shipLoc[0],shipLoc[1])
 
        #check to see if layout points need to be updated
        if currentlayout == 0 or currentlayout != shm.layout.state.get():

            rawlayout = shm.layout.state.get()
            currentlayout = rawlayout
            if len(rawlayout) > 0:
                self.OnPaint(1)


    def OnPaint(self, event):
        global currentlayout
        dc = wx.PaintDC(self)
        self.DoPrepareDC(dc)
        dc.SetBrush(wx.Brush((255,255,255)))
        dc.Clear()
        
        pen = wx.Pen((205,201,201), 1)
        dc.SetPen(pen)
     
        verticle_lines = [(i*scale,0,i*scale,75*scale) for i in range(75)]
        horizontal_lines = [(0,i*scale,75*scale,i*scale) for i in range(75)]
        dc.DrawLineList(horizontal_lines+verticle_lines)

        rawlayout = shm.layout.state.get()
        if len(rawlayout) > 0:
            betterlayout = pickle.loads(rawlayout)
            n_offset = betterlayout[0]
            e_offset = betterlayout[1]
            locations = betterlayout[2]
            npts = map(lambda x: x - n_offset, locations[0::2])
            epts = map(lambda x: -x - e_offset, locations[1::2])
            points = zip(npts, epts)
        
            for p in range(0, len(points)-1):
                DrawShapes(self, points[p], p)


    def OnLeftDown(self, event):
        self.scroller.Start(event.GetPosition())


    def OnLeftUp(self, event):
        self.scroller.Stop()
    

    def OnAbout(self,e):
        # Create a message dialog box
        dlg = wx.MessageDialog(self, " A position tracker \n in wxPython", "About Position Tracker", wx.OK)
        dlg.ShowModal() # Shows it
        dlg.Destroy() # finally destroy it when finished.
    
    
    def OnExit(self,e):
        self.Close(True)  # Close the frame.
    
    
    def setXYOff(self, event):
        global xoff
        global yoff
        global scale
        global xpad
        global ypad
        global xpoints
        global ypoints
        if (followSub == False):
            if (event == "shift"):
                xoff = scale * ((75/2.0) - xpoints[0] + xpad)
                yoff = scale * ((75/2.0) - ypoints[0] + ypad)
            elif (event == "zoom"):
               xoff = scale * ((75/2.0) - xpoints[0] + xpoints[len(xpoints) - 1] + xpad)
               yoff = scale * ((75/2.0) - ypoints[0] + ypoints[len(ypoints) - 1] + ypad)
        else:
            xoff = scale * ((75/2.0) + xpad)
            yoff = scale * ((75/2.0) + ypad)

def DrawShapes(self, location, it):
    global xoff
    global yoff
    global xpoints
    global ypoints
    dc = wx.PaintDC(self)
    if type(self) != legend:
        self.DoPrepareDC(dc)
    location = [location[0], location[1]]
    
    if it == 0: #start, yellow star
        shapePoints = [wx.Point(5,0),wx.Point(6,4),wx.Point(9,4),wx.Point(7,6),wx.Point(8,9),wx.Point(5,8),wx.Point(2,9),wx.Point(3,6),wx.Point(0,4),wx.Point(4,4)]
        pen = wx.Pen(wx.BLACK, 1)
        brush = wx.Brush((255,255,0))
        if type(self) != legend:
            xpoints[0] = float(location[0])
            ypoints[0] = float(location[1]) #need to add float or else you get embedded arrays
            xoff = scale*((75/2.0 - 19) - location[1] + xpad)
            yoff = scale*((75/2.0) - location[0] + ypad)
    elif it == 1: #buoy center, do nothing
        shapePoints = [wx.Point(0,0)]
        pen = wx.Pen(wx.BLACK, 1)
        brush = wx.Brush(wx.BLACK)
        location = [0, 0]
    elif it == 2: #orange buoy, orange circle
        shapePoints = [wx.Point(0,0)]
        dc.SetPen(wx.Pen((255, 69, 0), 1))
        dc.SetBrush(wx.Brush((255,69,0)))
        if type(self) != legend:
            dc.DrawCircle(location[1]*scale+xoff+5,location[0]*scale+yoff+5,5)
        else:
            dc.DrawCircle(location[1]+5, location[0]+5, 5)
        pen = wx.Pen(wx.BLACK, 1)
        brush = wx.Brush(wx.BLACK)
        location = [0, 0]
    elif it == 3: #yellow buoy, yellow circle
        shapePoints = [wx.Point(0,0)]
        dc.SetPen(wx.Pen((255, 255, 0), 1))
        dc.SetBrush(wx.Brush((255, 255, 0)))
        if type(self) != legend:
            dc.DrawCircle(location[1]*scale+xoff+5,location[0]*scale+yoff+5,5)
        else:
            dc.DrawCircle(location[1]+5, location[0]+5, 5)
        pen = wx.Pen(wx.BLACK, 1)
        brush = wx.Brush(wx.BLACK)
        location = [0, 0]
    elif it == 4: #green buoy, green circle
        shapePoints = [wx.Point(0,0)]
        dc.SetPen(wx.Pen((50, 205, 50), 1))
        dc.SetBrush(wx.Brush((50, 205, 50)))
        if type(self) != legend:
            dc.DrawCircle(location[1]*scale+xoff+5,location[0]*scale+yoff+5,5)
        else:
            dc.DrawCircle(location[1]+5, location[0]+5, 5)
        pen = wx.Pen(wx.BLACK, 1)
        brush = wx.Brush(wx.BLACK)
        location = [0, 0]
    elif it == 5: #wire, pink diamond
        shapePoints = [wx.Point(5,0),wx.Point(10,5),wx.Point(5,10),wx.Point(0,5)]
        pen = wx.Pen((255,20,147), 1)
        brush = wx.Brush((255,20,147))
    elif it == 6: #pipe to emperor, brown square
        shapePoints = [wx.Point(0,0),wx.Point(10,0),wx.Point(10,10),wx.Point(0,10)]
        pen = wx.Pen((139,69,19), 1)
        brush = wx.Brush((139,69,19))
    elif it == 7: #pipe to bins, purple triangle
        shapePoints = [wx.Point(0,9),wx.Point(4,0),wx.Point(9,9)]
        pen = wx.Pen((138,43,226), 1)
        brush = wx.Brush((138,43,226))
    elif it == 8: #torpedoes, red heart
        shapePoints = [wx.Point(1,0),wx.Point(3,0),wx.Point(4,2),wx.Point(5,2),wx.Point(6,0),wx.Point(8,0),wx.Point(9,4),wx.Point(5,9),wx.Point(4,9),wx.Point(0,4),wx.Point(0,1)]
        pen = wx.Pen((255,0,0), 1)
        brush = wx.Brush((255,0,0))
    elif it == 9: #pipe bins to emperor, light blue pentagon
        shapePoints = [wx.Point(5,0),wx.Point(9,3),wx.Point(7,9),wx.Point(7,9),wx.Point(2,9),wx.Point(0,3)]
        pen = wx.Pen((0,255,255), 1)
        brush = wx.Brush((0,255,255))
    elif it == 10: #pipe to wire, green diamond
        shapePoints = [wx.Point(5,0),wx.Point(10,5),wx.Point(5,10),wx.Point(0,5)]
        pen = wx.Pen((124,252,0), 1)
        brush = wx.Brush((124,252,0))
    elif it == 11: #emperor, lavender box
        shapePoints = [wx.Point(0,0),wx.Point(10,0),wx.Point(10,10),wx.Point(0,10)]
        pen = wx.Pen((216,191,216), 1)
        brush = wx.Brush((216,191,216))    
    dc.SetPen(pen)
    dc.SetBrush(brush)
    if type(self) != legend:
        location = worldToScreen(location[1], location[0], "drawShape")
        dc.DrawPolygon(shapePoints, location[0], location[1])
    else:
        dc.DrawPolygon(shapePoints, location[1], location[0])


def worldToScreen(north,east,eventName):
    if eventName == "drawShape":
        location = [north * scale + xoff, east * scale + yoff]
    elif eventName == "drawPath":
        location = [xoff + scale * (north - 19), yoff + scale * east]
    elif eventName == "drawSub":
        if (followSub == False):
            location = [(north - 19) * scale + xoff, east*scale + yoff]
        else:
            location = [xoff + scale * (north - 19), yoff + scale * east]

    return location


#-------------------------------------------------------------------------------
if __name__ == '__main__':
    import sys,os
    app = wx.App(False)
    frame = wx.Frame(None, -1, "Position Tracker", pos=(50,50), size=(640,480), style=wx.DEFAULT_FRAME_STYLE)
    legendframe = wx.Frame(None)
    DragScrollerExample(frame, -1)
    app.MainLoop()



