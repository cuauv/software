from gi.repository import Gtk, Gdk, GLib
import cairo
import math
import shm
import user_config as conf
import pickle

from shm import navigation_desires
from var_util import SmoothVar

from misc.log import with_logging

MAX_ZOOM = 8
MIN_ZOOM = 0.3
REFRESH = 1./25 #effectively sets the draw framerate

@with_logging
class Canvas(Gtk.DrawingArea):
    def __init__ (self):
        Gtk.DrawingArea.__init__(self)
        self.connect('draw', self._do_expose)
        self.set_events(Gdk.EventMask.BUTTON_PRESS_MASK)
        self.connect('button-press-event', self.on_button_press)
        self.follow = False
        self.followHeading = False
        self.killed = False
        self.last_pos = [self.sub_pos()]
        
        self.refresh_matrix()
        self.smooth_zoom = None
        self.smooth_rot = None
        self.smooth_pan = None
        self.refresh()
        self.i = 0

    def on_button_press(self, widget, event):
        xx, yx, xy, yy, x0, y0 = self.grid_mat
        x, y = event.x, event.y
        dx, dy = x0 - x, y - y0 + 17 #Weird offset 
        dx_prime, dy_prime = xx*dx + xy*dy, yx*dx + yy*dy
        navigation_desires.north.set(dy_prime/conf.MAGNIFY)
        navigation_desires.east.set(dx_prime/conf.MAGNIFY)


    def _do_expose(self, widget, cr):
        ''' This method is called internally, if you want to redraw the canvas,
            call self.redraw()
            Every draw, this method is called, consider it the starting point.
        '''
        allocation = self.get_allocation()
        self.width = allocation.width
        self.height = allocation.height
        # This width / height is the current size of the canvas
        # If the canvas is resized, these values will changed (and
        # this do_expose method will be triggered)

        self.set_transform(cr)
        self.draw_grid(cr)
        self.draw_sub(cr)
        # Disabled because the needed shm variables are now gone :(
        #self.draw_tags(cr)
        self.i += 1

    def redraw(self):
        self.queue_draw()

    def refresh(self):
        ''' Run to periodically refresh the display '''
        self.redraw()
        if not self.killed:
            GLib.timeout_add(int(REFRESH*1000), self.refresh)

    def kill(self):
        self.killed = True

    def refresh_matrix(self):
        self.scale = 1.0
        self.grid_mat = cairo.Matrix()
        self.pan_tup = (0,0)

    def set_transform(self, cr):
        ''' Handles affine transformations for the canvas matrix
            Shifts the display to the center of the display and adjusts for
            any user pan
        '''
        xx, yx, xy, yy, x0, y0 = self.grid_mat
        x0 = self.width/2 + self.pan_tup[0]
        y0 = self.height/2 + self.pan_tup[1]
        self.grid_mat = cairo.Matrix(xx, yx, xy, yy, x0, y0)
        self.set_follow(cr)

    def _ticker(self):
        ''' Ticks periodically while there exists some variable that needs
            ticking '''
        if self.smooth_zoom is not None:
            if self.smooth_zoom.running():
                self.smooth_zoom.tick()
                self.zoom(None)
                GLib.timeout_add(int(conf.PERIOD*1000), self._ticker)
            else:
                self.smooth_zoom = None
        if self.smooth_rot is not None:
            if self.smooth_rot.running():
                self.smooth_rot.tick()
                self.rotate(None)
                GLib.timeout_add(int(conf.PERIOD*1000), self._ticker)
            else:
                self.smooth_rot = None
        if self.smooth_pan is not None:
            if self.smooth_pan.running():
                self.smooth_pan.tick()
                self.pan(None, None)
                GLib.timeout_add(int(conf.PERIOD*1000), self._ticker)
            else:
                self.smooth_pan = None

    def set_follow(self, cr):
        ''' Changes display matrix if sub is being followed '''
        if self.follow:
            sub_head = self.sub_head()
            xx, yx, xy, yy, x0, y0 = self.grid_mat
            rot_mat = cairo.Matrix(xx, yx, xy, yy, self.width/2, self.height/2)
            if self.followHeading:
                rot_mat.rotate(sub_head*math.pi/180.)
            x,y = self.sub_pos()
            rot_mat.translate(-x, -y)
            cr.set_matrix(rot_mat)
        else:
            cr.set_matrix(self.grid_mat)

    def pan(self, x, y):
        ''' Shift the screen by x, y '''
        if self.follow:
            return
        if conf.SMOOTHING:
            if x is None and y is None:
                d = self.smooth_pan.value
                (x, y) = self.smooth_pan.initial
            else:
                d = 1.
                self.smooth_pan = SmoothVar(d/SmoothVar.steps, thresh=0.2/SmoothVar.steps, initial=(x,y))
                self.smooth_pan.setDesire(0)
                self.smooth_pan.setVelocity(0.02)
                factor = self.smooth_pan.value
                self._ticker()
        else:
            d = 1
        self.pan_tup = (self.pan_tup[0]+x*d,self.pan_tup[1]+y*d)
        self.redraw()

    def center(self):
        self.refresh_matrix()
        self.redraw()

    def zoom(self, factor):
        if conf.SMOOTHING:
            if factor is None:
                factor = self.smooth_zoom.value
                if self.smooth_zoom.value > 1 and self.scale > self.smooth_zoom.final \
                    or self.smooth_zoom.value < 1 and self.scale < self.smooth_zoom.final:
                    self.smooth_zoom = None
            else:
                self.smooth_zoom = SmoothVar(1-(1-factor)/SmoothVar.steps)
                self.smooth_zoom.setDesire(1)
                self.smooth_zoom.setFinal(self.scale*factor)
                factor = self.smooth_zoom.value
                self._ticker()

        if not (self.scale > MAX_ZOOM and factor > 1 or \
                self.scale < MIN_ZOOM and factor < 1):
            self.grid_mat.scale(factor, factor)
            self.scale *= factor
            self.redraw()
        else:
            self.smooth_zoom = None
            self.log.info("Restraint is a virtue.")

    def rotate(self, factor):
        if conf.SMOOTHING:
            if factor is None:
                factor = self.smooth_rot.value
            else:
                self.smooth_rot = SmoothVar(factor/SmoothVar.steps, thresh=0.1/SmoothVar.steps, initial=0)
                self.smooth_rot.setDesire(0)
                factor = self.smooth_rot.value
                self._ticker()
        if not self.follow:
            self.grid_mat.rotate(factor)
            self.redraw()

    def follow_sub(self, followHeading):
        self.followHeading = followHeading
        self.follow = not self.follow
        if not self.follow:
            self.center()
        self.redraw()

    def reset_path(self):
        self.last_pos = []
        self.redraw()

    def draw_grid(self, cr):
        ''' Draws many lines for the grid '''
        cr.set_source_rgba(*conf.GRID_COLOR)
        num = conf.GRID_LINES
        space = conf.GRID_SPACE * conf.MAGNIFY
        cr.set_line_width(2/self.scale)
        for i in xrange(-num, num+1):
            cr.move_to(i*space, -space*num)
            cr.line_to(i*space, space*num)
        for i in xrange(-num, num+1):
            cr.move_to(-space*num, i*space)
            cr.line_to(space*num, i*space)
        cr.stroke()
        cr.save()

    def sub_pos(self):
        ''' Get sub's translated position, with respect to canvas
            MAGNIFY scales the shm variables to reasonable pixel distances'''
        x = conf.MAGNIFY*shm.kalman.north.get()
        y = conf.MAGNIFY*shm.kalman.east.get()
        return (-y, x)

    def sub_head(self):
        ''' Get sub's translated heading ''' 
        return 180-shm.kalman.heading.get()

    def draw_sub(self, cr):
        # TODO: show scale... meter/pixel ratio?
        ''' Draw the sub at position x, y, which come from shm
            1. translates matrix to -x, -y
            2. Rotates to match heading
            3. Un-rotates, and translates back to 0,0 '''
        # Draws path
        if len(self.last_pos) > 0:
            cr.push_group()
            cr.move_to(*self.last_pos[0])
            cr.set_line_width(2/self.scale)
            for p in self.last_pos:
                cr.line_to(*p)
            if len(self.last_pos) > 1:
                cr.line_to(*self.sub_pos())
                cr.stroke()
            path_pattern = cr.pop_group()
            cr.set_source_rgba(*conf.PATH_COLOR)
            cr.mask(path_pattern)

        cr.set_source_rgba(*conf.SUB_COLOR)

        # Draws the sub triangle figure
        x, y = self.sub_pos()
        cr.push_group()
        cr.move_to(x, y)
        cr.line_to(x-conf.SUB_SIZE/2, y-conf.SUB_SIZE)
        cr.line_to(x, y+conf.SUB_SIZE)
        cr.line_to(x+conf.SUB_SIZE/2, y-conf.SUB_SIZE)
        cr.line_to(x-conf.SUB_SIZE/2, y-conf.SUB_SIZE)
        cr.close_path()
        cr.fill()
        sub_pattern = cr.pop_group()

        # Rotates/translates and plots the sub
        angle = self.sub_head()*math.pi/180.
        sub_mat = sub_pattern.get_matrix()
        rot_mat = cairo.Matrix.init_rotate(angle + math.pi)
        sub_mat = cairo.Matrix.multiply(rot_mat, sub_mat)
        phi = -math.atan2(y, x)
        theta = math.pi - angle
        r = math.sqrt(x*x + y*y)
        dx = r * math.cos(phi-theta) - x
        dy = r * math.sin(-phi+theta) - y

        sub_mat.translate(dx, dy)
        xx, yx, xy, yy, x0, y0 = sub_mat
        sub_mat.translate(0, 0)
        sub_pattern.set_matrix(sub_mat)
        cr.set_source_rgba(*conf.SUB_COLOR)
        cr.mask(sub_pattern)
        '''
        # pngs??
        sub_surface = cairo.ImageSurface.create_from_png('trogdor.png')
        sub = cairo.SurfacePattern(sub_surface)
        sub.set_matrix(sub_mat)
        cr.mask(sub)
        '''

        self.add_point(self.sub_pos())


    def add_point(self, p):
        ''' Adds a point to the path only if it is within the path resolution
            Also trims the path if needed '''
        if len(self.last_pos) == 0:
            self.last_pos.append(p)
        elif self.dist(self.last_pos[-1],p) > 1./conf.PATH_RES:
            self.last_pos.append(p)

        if conf.PATH_LEN >= 0 and len(self.last_pos) > conf.PATH_LEN:
            del self.last_pos[0]

    def dist(self, a,b):
        return math.sqrt((a[0]-b[0])**2+(a[1]-b[1])**2)

    def draw_tags(self, cr):
        points_to_draw = []

        # Add tag
        points_to_draw.append(((shm.mission.last_forward.get(), shm.mission.last_sway.get()), conf.TAG_COLOR))

        # Add layout points
        arx = shm.layout.state.get()
        if len(arx) > 0:
            thg = pickle.loads(arx)
            n_offset = thg[0]
            e_offset = thg[1]
            ar = thg[2]
            npts = map(lambda x: x - n_offset, ar[0::2])
            epts = map(lambda x: x - e_offset, ar[1::2])
            pts = zip(npts, epts) 
            for pt in pts:
                points_to_draw.append((pt, conf.ELEMENT_COLOR))


        for pt, clr in points_to_draw:
            cr.set_source_rgba(*clr)
            y = pt[0] * conf.MAGNIFY #east
            x = -pt[1] * conf.MAGNIFY #north
            cr.rectangle(x-conf.TAG_SIZE/2.,y-conf.TAG_SIZE/2., conf.TAG_SIZE, conf.TAG_SIZE)
            cr.fill()

