#!/usr/bin/env python2

from auval.vehicle import *
import gtk, gobject
from random import randint
from math import *
import shm
import pickle

window_size = 500
pixels_per_meter = 25
initial_rotation = 180

north_source = "dmg_north"
east_source = "dmg_east"

class DrawingArea(gtk.DrawingArea):
    def __init__(self):
        gtk.DrawingArea.__init__(self)
        self.connect("expose_event", self.on_expose)
        self.connect("button_press_event", self.on_mouse_click)        
        self.set_events(gtk.gdk.EXPOSURE_MASK | gtk.gdk.LEAVE_NOTIFY_MASK | gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.POINTER_MOTION_MASK | gtk.gdk.POINTER_MOTION_HINT_MASK)
        self.set_size_request(window_size,window_size)
        self.rotation = radians(180)
        self.reset()
        gobject.timeout_add(500, self.add_line)

        

    def on_expose(self, widget, event):
        drawable = self.window
        drawable.draw_lines(self.line_gc, self.rotated_points)
        
        meter_scale = [(5,5), (5,10), (pixels_per_meter+5,10), (pixels_per_meter+5,5)];
        drawable.draw_lines(self.line_gc, meter_scale)

        ship_polygon = [(0,10), (5,-5), (-5,-5)]
        summed_angle = radians(shared_vars["heading"].get() - 180) - self.rotation
        ship_polygon = [(int(point[0]*cos(summed_angle) - point[1]*sin(summed_angle)) + self.rotated_points[-1][0], int(point[0]*sin(summed_angle) + point[1]*cos(summed_angle)) + self.rotated_points[-1][1]) for point in ship_polygon]
        drawable.draw_polygon(self.polygon_gc, True, ship_polygon)
        
        
        tagLocation_poly = [(0,3), (3,0), (0,-3), (-3, 0)]
        tagPos = (pixels_per_meter * (shared_vars["last_sway"].get() - self.origin_x), pixels_per_meter*(shared_vars["last_forward"].get() - self.origin_y))
        tagPos = (tagPos[0]*cos(self.rotation) - tagPos[1]*sin(self.rotation), tagPos[0]*sin(self.rotation) + tagPos[1]*cos(self.rotation))
        tagPos = (tagPos[0] + window_size/2, window_size/2 - tagPos[1])
        tagLocation_poly = [(int(point[0]+tagPos[0]), int(point[1]+tagPos[1])) for point in tagLocation_poly]
        
        drawable.draw_polygon(self.tagLocation_gc, True, tagLocation_poly)

        # Draw waypoint just as tag but different spot
        waypointLocation_poly = [(0,3), (3,0), (0,-3), (-3, 0)]
        wayptPos = (pixels_per_meter * (shm.mission.waypoint_east.get() - self.origin_x), pixels_per_meter*(shm.mission.waypoint_north.get() - self.origin_y))
        wayptPos = (wayptPos[0]*cos(self.rotation) - wayptPos[1]*sin(self.rotation), wayptPos[0]*sin(self.rotation) + wayptPos[1]*cos(self.rotation))
        wayptPos = (wayptPos[0] + window_size/2, window_size/2 - wayptPos[1])
        waypointLocation_poly = [(int(point[0]+wayptPos[0]), int(point[1]+wayptPos[1])) for point in waypointLocation_poly]
        drawable.draw_polygon(self.waypointLocation_gc, True, waypointLocation_poly)
 
        locator1_poly = [(0,3), (3,0), (0,-3), (-3, 0)]
        tagPos = (pixels_per_meter * (shared_vars["locator1_east"].get() - self.origin_x), pixels_per_meter*(shared_vars["locator1_north"].get() - self.origin_y))
        tagPos = (tagPos[0]*cos(self.rotation) - tagPos[1]*sin(self.rotation), tagPos[0]*sin(self.rotation) + tagPos[1]*cos(self.rotation))
        tagPos = (tagPos[0] + window_size/2, window_size/2 - tagPos[1])
        locator1_poly = [(int(point[0]+tagPos[0]), int(point[1]+tagPos[1])) for point in locator1_poly]
        
        drawable.draw_polygon(self.locator1_gc, True, locator1_poly)

 
        locator2_poly = [(0,3), (3,0), (0,-3), (-3, 0)]
        tagPos = (pixels_per_meter * (shared_vars["locator2_east"].get() - self.origin_x), pixels_per_meter*(shared_vars["locator2_north"].get() - self.origin_y))
        tagPos = (tagPos[0]*cos(self.rotation) - tagPos[1]*sin(self.rotation), tagPos[0]*sin(self.rotation) + tagPos[1]*cos(self.rotation))
        tagPos = (tagPos[0] + window_size/2, window_size/2 - tagPos[1])
        locator2_poly = [(int(point[0]+tagPos[0]), int(point[1]+tagPos[1])) for point in locator2_poly]

        drawable.draw_polygon(self.locator2_gc, True, locator2_poly)


        #Layout points!
        arx = shm.layout.state.get()
        if len(arx) > 0:
            thg = pickle.loads(arx)
            n_offset = thg[0]
            e_offset = thg[1]
            ar = thg[2]
            npts = map(lambda x: x - n_offset, ar[0::2])
            epts = map(lambda x: x - e_offset, ar[1::2])

            pts = zip(npts, epts) 

            #For every layout point, draw it
            for pt in pts:
                north = pt[0]
                east = pt[1]
                layout_poly = [(0,3), (3,0), (0,-3), (-3, 0)]
                tagPos = (pixels_per_meter * (east - self.origin_x), pixels_per_meter*(north - self.origin_y))
                tagPos = (tagPos[0]*cos(self.rotation) - tagPos[1]*sin(self.rotation), tagPos[0]*sin(self.rotation) + tagPos[1]*cos(self.rotation))
                tagPos = (tagPos[0] + window_size/2, window_size/2 - tagPos[1])
                layout_poly = [(int(point[0]+tagPos[0]), int(point[1]+tagPos[1])) for point in layout_poly]
                
                drawable.draw_polygon(self.layout_gc, True, layout_poly)

        return False
    
    def reset(self):
        self.origin_x = shared_vars[east_source].get()
        self.origin_y = shared_vars[north_source].get()
        self.raw_points = [(0,0)]
        self.rotated_points = [(window_size/2, window_size/2)]

    def update_rotation(self):
        self.rotated_points = [(int(point[0]*cos(self.rotation) - point[1] * sin(self.rotation)) + window_size/2, (window_size/2)-int(point[0] * sin(self.rotation) + point[1] * cos(self.rotation))) for point in self.raw_points]
        self.queue_draw()

    def on_mouse_click(self, widget, event):
        tagPos = (event.x - window_size/2, window_size/2 - event.y)
        tagPos = (tagPos[0]*cos(self.rotation) + sin(self.rotation)*tagPos[1], -1 * sin(self.rotation)*tagPos[0] + cos(self.rotation)*tagPos[1])
        tagPos = (tagPos[0] / pixels_per_meter + self.origin_x, tagPos[1] / pixels_per_meter + self.origin_y)

        shared_vars["last_sway"].set(tagPos[0])
        shared_vars["last_forward"].set(tagPos[1])
        
        widget.queue_draw()


    def on_key_press(self, widget, event):
        if gtk.gdk.keyval_name(event.keyval) == 'r':
            self.reset()
            self.queue_draw()
        elif gtk.gdk.keyval_name(event.keyval) == 't': #Tag!
            tagLocation()    
        #elif gtk.gdk.keyval_name(event.keyval) == 'a':
        #    shared_vars[north_source].set(randint(-10,10))
        #    shared_vars[east_source].set(randint(-10,10))
        #    self.add_line()
        elif gtk.gdk.keyval_name(event.keyval) == 'p':
            print degrees(self.rotation)
        elif event.keyval == 65361:
            self.rotation += radians(1)
            self.update_rotation()
        elif event.keyval == 65363:
            self.rotation -= radians(1)
            self.update_rotation()
        return True
    
    def add_line(self):
        dx = (shared_vars[east_source].get() - self.origin_x) * pixels_per_meter
        dy = (shared_vars[north_source].get() - self.origin_y) * pixels_per_meter
        if (dx,dy) != self.raw_points[-1]:
            self.raw_points.append((dx,dy))
            new_value = int(dx * cos(self.rotation) - dy * sin(self.rotation)) + window_size/2, (window_size/2)-int(dx * sin(self.rotation) + dy * cos(self.rotation))
            self.rotated_points.append(new_value)
            self.queue_draw()
        return True
    
    def set_colors(self):
        self.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("lightblue"))
        self.line_gc = self.window.new_gc()
        self.line_gc.fill = gtk.gdk.SOLID
        self.line_gc.foreground = self.get_colormap().alloc_color('black')
        self.line_gc.line_width = 3
        self.line_gc.join_style = gtk.gdk.JOIN_ROUND

        self.polygon_gc = self.window.new_gc()
        self.polygon_gc.foreground = self.get_colormap().alloc_color('red')
        
        self.tagLocation_gc = self.window.new_gc()
        self.tagLocation_gc.foreground = self.get_colormap().alloc_color('yellow')

        self.waypointLocation_gc = self.window.new_gc()
        self.waypointLocation_gc.foreground = self.get_colormap().alloc_color('orange')

        self.locator1_gc = self.window.new_gc()
        self.locator1_gc.foreground = self.get_colormap().alloc_color('blue')

        self.locator2_gc = self.window.new_gc()
        self.locator2_gc.foreground = self.get_colormap().alloc_color('purple')

        self.layout_gc = self.window.new_gc()
        self.layout_gc.foreground = self.get_colormap().alloc_color('darkgreen')

def tagLocation():
    shm.mission.last_sway.set( shm.kalman.east.get() )
    shm.mission.last_forward.set( shm.kalman.north.get() )

def main():
    window = gtk.Window()
    W = DrawingArea()
    window.add(W)
    window.connect("destroy", gtk.main_quit)
    window.connect("key_press_event", W.on_key_press)
    window.show_all()
    W.set_colors()
    gtk.main()

if __name__ == "__main__":
    main()
