from gi.repository import Gtk, Gdk, Pango, PangoCairo
import cairo
import math
import sys
from itertools import groupby
import operator

from misc.log import with_logging


SCROLL_HEIGHT=15
SCROLL_HEAD_WIDTH=9
MIN_FRAME_VIEW = 20
TICK_HEIGHT=20

SCROLL_BAR_COLOR = (0.6, 0.6, 0.6)
SCROLL_HEAD_COLOR = (0.4, 0.4, 0.4)
TICK_COLOR = (0.6, 0.6, 0.6)

CURSOR_COLOR = (1.0, 0, 0)
CURSOR_TRIANGLE_COLOR = (0.8,0.7,0)
CURSOR_TRIANGLE_WIDTH=10

DATA_Y = 25
DATA_HEIGHT = 15
DATA_COLOR = (0.0, 0.3, 0.8)
DATA_POS = (0.0, 0.8, 0.0)
DATA_NEG = (0.8, 0.0, 0.0)
DATA_RESULT_HEIGHT = 5

PREVIEW_Y = 45
PREVIEW_HEIGHT = 45
PREVIEW_WIDTH = 60

X_BETWEEN_LABELS = 80

LOOP_SELECTING_COLOR = (0.6, 0.0, 0.5)
LOOP_SELECTED_COLOR = (0.0, 0.6, 0.5)

LOOP_BAR_Y = 15
LOOP_BAR_HEIGHT = 5

class CS: #Click state
    SBAR = 0  #Scrollbar clicked
    LSBAR = 1 #Left resize clicked
    RSBAR = 2 #Right resize clicked
    CURSOR = 3 #Cursor move indicated
    MAIN = 4 #Main area clicked
    NONE = 5  #Nothing of interest clicked

ZOOM_FACTOR = 1.5
RANGE_PAD_FACTOR = 1.2

@with_logging
class Timeline(Gtk.DrawingArea):
    """
    Timeline widget for the CAVE GUI
    Wraps drawing area using cairo
    """
    def __init__ (self, parent):
        self.parent = parent
        self.enabled = False
        Gtk.DrawingArea.__init__(self)
        self.connect('draw', self._do_expose)
        self.set_events(Gdk.EventMask.BUTTON_PRESS_MASK | Gdk.EventMask.BUTTON_MOTION_MASK | Gdk.EventMask.SCROLL_MASK |
                Gdk.EventMask.BUTTON_RELEASE_MASK)
        self.connect("button-press-event", self._click) #mouse button is pressed (either left or right)
        self.connect("motion-notify-event", self._drag) #mouse is held down and in motion
        self.connect("button-release-event", self._release) #button was released
        #Motion notify seems to only react when the mouse is down; this doesn't match the documentation exactly;
        #might be relying on a side-effect #TODO: use proper drag events
        self.connect("scroll-event", self._scroll)
        self.set_size_request (400, 125)

        self.x = 90
        self.y = 5

        self.view_start = 0 #Frame where view starts
        self.view_end = 100 #Frame where view ends

        self.length = 100 #Number of frames within the timeline (can be changed)

        self.cursor = 0 #Frame at which to display the cursor

        self.available_width = None

        #loop vars
        self.loop_start = 0
        self.loop_start_temp = None
        self.loop_dragging = False
        self.loop_end = float('nan')
        self.loop_enabled = False
        self.loop_set = False

        #Callbacks
        self.cursor_change = None

        #preview manager for video thumbnails
        self.video_preview_manager = None

        #Internal state
        self.cs = CS.NONE
        self.origin_mouse = (0,0)
        self.origin_start = 0
        self.origin_end = 0

    def set_loop_enabled(self, enabled):
        self.loop_enabled = enabled
        self.loop_set = False
        self.loop_start = 0
        self.loop_end = float('nan')
        if enabled:
            self.log.info("Loop enabled. Drag the cursor in order to establish a loop region.")
        else:
            self.log.info("Loop disabled.")
        self.queue_draw()

    def get_loop_bounds(self):
        return (self.loop_start, self.loop_end)

    def set_preview_manager(self, pm):
        self.video_preview_manager = pm
        self.video_preview_manager.frame_height = PREVIEW_HEIGHT

    def set_length(self, length):
        #Set a new frame length for the timeline (useful when switching videos)
        self.length = length
        self.view_start = 0
        self.view_end = length
        self.cursor = 0
        self.queue_draw()

    def zoom(self, factor):
        width = max(abs(self.cursor - self.view_end) * 2, abs(self.cursor - self.view_start) * 2)
        new_width = width * factor
        vs = self.cursor - int(new_width / 2)
        ve = self.cursor + int(new_width / 2)
        vs = max(0, vs)
        ve = min(self.length, ve)
        if ve - vs < MIN_FRAME_VIEW:
            return
        self.view_start = vs
        self.view_end = ve
        self.queue_draw()

    #Ask the timeline to display an indicated range
    #Will display this range with some padding on either side if desired
    #Also handles corner cases
    def set_range(self, start, end, padding=True):
        r_new = (end - start)
        center = (start + end) / 2
        if padding:
            r_new = RANGE_PAD_FACTOR * r_new
        if r_new < MIN_FRAME_VIEW:
            r_new = MIN_FRAME_VIEW
        vs = center - r_new / 2
        vs = int(max(0, min(vs, self.length)))
        ve = center + r_new / 2
        ve = int(max(0, min(ve, self.length)))
        if vs == 0:
            ve = int(r_new)
        if ve == self.length:
            vs = int(self.length - r_new)
        self.view_start = max(0, vs)
        self.view_end = min(ve, self.length)
        self.cursor = max(0, min(self.length, start))
        self.queue_draw()

    def zoom_in(self):
        self.zoom(1.0 / ZOOM_FACTOR)

    def zoom_out(self):
        self.zoom(ZOOM_FACTOR)

    def _scroll(self, area, event):
        if event.direction == Gdk.ScrollDirection.UP:
            self.zoom_in()
        elif event.direction == Gdk.ScrollDirection.DOWN:
            self.zoom_out()

    def _click(self, area, event):
        if not self.enabled:
            return
        a = self.get_allocation()
        self.origin_mouse = (event.x, event.y)
        self.origin_start = self.view_start
        self.origin_end = self.view_end

        #Classify the click and assign click state (so that drag knows how to operate)
        if event.y >= a.height - SCROLL_HEIGHT: #Somewhere in scrollbar area clicked
            x_start = float(self.view_start) / self.length * a.width
            x_end = float(self.view_end) / self.length * a.width
            if event.x > x_start and event.x < x_start + SCROLL_HEAD_WIDTH: #left bar clicked
                self.cs = CS.LSBAR
            elif event.x < x_end and event.x > x_end - SCROLL_HEAD_WIDTH: #right bar clicked
                self.cs = CS.RSBAR
            else: #somewhere else in scrollbar clicked
                self.cs = CS.SBAR
        elif event.y <= TICK_HEIGHT: #scrollbar area clicked
            self.cs = CS.CURSOR
            self._change_cursor(event.x)
            self.queue_draw()
            if self.loop_enabled:
                start_frame = self.get_frame(event.x)
                self.loop_start_temp = start_frame
                self.loop_dragging = False

        else: #Clicked the main region
            self.cs = CS.MAIN

    def _release(self, area, event):
        if (self.cs == CS.CURSOR) and self.loop_enabled and self.loop_dragging:
            end_frame = self.get_frame(event.x)
            self.loop_end = end_frame
            if self.loop_start > self.loop_end:
                t = self.loop_end
                self.loop_end = self.loop_start
                self.loop_start = t
            self.loop_set = True
            self.log.info("Loop now set from %d to %d" % (self.loop_start, self.loop_end))
            self.queue_draw()
        self.loop_dragging = False
        self.loop_start_temp = None

        self.cs = CS.NONE

    def move_cursor(self, new_frame):
        self.cursor = new_frame

        # auto cursor follow functionality
        if self.cursor == self.view_end + 1 and self.cs == CS.NONE:
            self.set_range(self.view_end, (self.view_end + (self.view_end - self.view_start)), padding=False)

        self.queue_draw()

    def _change_cursor(self, x_pos):
        new_cursor = self.get_frame(x_pos)
        self.cursor = new_cursor
        #TODO: Signal movie player to change
        if self.cursor_change is not None:
            self.cursor_change(self.cursor)

        if self.loop_start_temp is not None and self.loop_start_temp != self.cursor:
            self.loop_dragging = True
            self.loop_set = False
            self.loop_start = self.loop_start_temp

    def get_frame(self, x_pos):
        p = (float(x_pos) / self.get_allocation().width)
        num_frames = (self.view_end - self.view_start)
        new_frame = self.view_start + int(math.floor(num_frames * p))
        new_frame = max(min(new_frame, self.view_end), self.view_start)
        return new_frame

    #This should be used more often
    def get_x_pos(self, frame):
        assert(self.available_width)
        return int((float(frame) - self.view_start) / (self.view_end - self.view_start) * self.available_width)

    def _drag(self, area, event):
        if not self.enabled:
            return
        a = self.get_allocation()

        dx = event.x - self.origin_mouse[0]
        dframes = int(dx / a.width * self.length)

        dlocalframes = -1 * int(dx / a.width * (self.origin_end - self.origin_start))

        #Click and drag on the main area to seek
        if self.cs == CS.MAIN:
            ns = self.origin_start + dlocalframes
            ne = self.origin_end + dlocalframes
            if ns >= 0 and ne <= self.length:
                self.view_start = ns
                self.view_end = ne

        ### Scrollbar
        if self.cs == CS.SBAR:
            ns = self.origin_start + dframes
            ne = self.origin_end + dframes
            if ns >= 0 and ne <= self.length:
                self.view_start = ns
                self.view_end = ne
        if self.cs == CS.LSBAR:
            ns = self.origin_start + dframes
            if ns >= 0 and self.view_end - ns > MIN_FRAME_VIEW:
                self.view_start = ns
        if self.cs == CS.RSBAR:
            ne = self.origin_end + dframes
            if ne <= self.length and ne - self.view_start > MIN_FRAME_VIEW:
                self.view_end = ne

        ### Cursor
        if self.cs == CS.CURSOR:
            self._change_cursor(event.x)

        self.queue_draw()

    def preview_callback(self, frame_number, video):
        #TODO: Unused params
        if video != self.parent.video_box.video:
            return
        self.queue_draw()

    def _do_expose(self, widget, cr):

        allocation = self.get_allocation()
        width = allocation.width
        height = allocation.height
        self.available_width = width

        #Draw frame ticks
        ticks = list(enumerate(range(self.view_start, self.view_end)))

        cr.set_source_rgb(*TICK_COLOR)
        if len(ticks) < 1500:
            cr.set_line_width(1.0)
            for i,label in ticks:
                x_pos = float(i) / len(ticks) * width
                cr.move_to(x_pos, 0)
                cr.line_to(x_pos, TICK_HEIGHT)
            cr.stroke()
        else:
            #There are so many frames that we can just draw a massive rectangle at this point
            cr.rectangle(0,0, width, TICK_HEIGHT)
            cr.fill()

        #Label some of them ticks
        cr.set_source_rgb(0.3,0.3,0.3)
        cr.set_font_size(10)
        cr.select_font_face("Georgia",
                                 cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        label_frames = int(float(X_BETWEEN_LABELS) / width * (self.view_end - self.view_start))
        i = int(self.view_start / label_frames) * label_frames
        while i < self.view_end:
            x_pos = int((float(i) - self.view_start) / (self.view_end - self.view_start) * width)
            cr.move_to(x_pos, TICK_HEIGHT / 2)
            cr.show_text(str(i))
            i += label_frames

        def draw_frame_range(frame_list, height):
            #Code to get contiguous ranges from this list (to reduce drawing time)
            #ref: http://code.activestate.com/recipes/496682-make-ranges-of-contiguous-numbers-from-a-list-of-i/
            frame_list.sort()
            for k,g in groupby(enumerate(frame_list), lambda (i,x):i-x):
                segment = map(operator.itemgetter(1),g)
                first_frame = segment[0]
                last_frame = segment[-1]

                #Draw box for the tag segment
                x_start = int(float(first_frame - self.view_start) / len(ticks) * width)
                x_end = int(float(last_frame + 1 - self.view_start) / len(ticks) * width)
                cr.rectangle(x_start,DATA_Y, x_end - x_start, height)
                cr.fill()

        #Draw the tag data visible in the current view
        tag_select = self.parent.video_tree_manager.get_selected_tag()
        if tag_select is not None:
            cr.set_source_rgb(*DATA_COLOR)
            frame_list = tag_select.get_frame_list(frame_min=self.view_start, frame_max=self.view_end)
            draw_frame_range(frame_list, DATA_HEIGHT)

            cr.set_source_rgb(*DATA_POS)
            draw_frame_range(tag_select.test_pos, DATA_RESULT_HEIGHT)

            cr.set_source_rgb(*DATA_NEG)
            draw_frame_range(tag_select.test_neg, DATA_RESULT_HEIGHT)

        #Draw the image previews
        requested_frames = []
        preview_frames = int(float(PREVIEW_WIDTH) / width * (self.view_end - self.view_start))
        i = int(self.view_start / preview_frames) * preview_frames
        while i < self.view_end:
            frame = self.video_preview_manager.request_frame(self.parent.video_box.video, i)
            requested_frames.append(i)
            if frame is not None:
                x = int((float(i) - self.view_start) / (self.view_end - self.view_start) * width)
                cr.set_source_surface(frame, x, PREVIEW_Y)
                cr.paint()
            i += preview_frames
        self.video_preview_manager.clear_all_requests_except(requested_frames)

        #Draw the loop bar
        if self.loop_enabled:
            lc = None
            if self.loop_set:
                lx = self.get_x_pos(self.loop_start)
                lx2 = self.get_x_pos(self.loop_end)
                lc = LOOP_SELECTED_COLOR
            elif self.cs == CS.CURSOR and self.loop_dragging:
                lx = self.get_x_pos(self.loop_start)
                lx2 = self.get_x_pos(self.cursor)
                lc = LOOP_SELECTING_COLOR
            if lc is not None:
                cr.set_source_rgb(*lc)
                cr.rectangle(lx, LOOP_BAR_Y, lx2 - lx, LOOP_BAR_HEIGHT)
                cr.fill()


        #Draw cursor
        if self.cursor >= self.view_start and self.cursor < self.view_end:
            x = int(float(self.cursor - self.view_start) / len(ticks) * width)
            cr.set_line_width(1.0)
            cr.set_source_rgb(*CURSOR_COLOR)
            cr.move_to(x,0)
            cr.line_to(x, height-SCROLL_HEIGHT)
            cr.stroke()
            cr.set_source_rgb(*CURSOR_TRIANGLE_COLOR)
            cr.move_to(x - CURSOR_TRIANGLE_WIDTH/2, 0)
            cr.line_to(x + CURSOR_TRIANGLE_WIDTH/2, 0)
            cr.line_to(x, TICK_HEIGHT / 2)
            cr.line_to(x - CURSOR_TRIANGLE_WIDTH/2, 0)
            cr.fill()

        #Draw scrollbar "head"
        cr.set_line_width(2.0)
        cr.set_source_rgb(*SCROLL_BAR_COLOR)
        x_start = float(self.view_start) / self.length * width
        x_end = float(self.view_end) / self.length * width
        cr.rectangle(x_start, height-SCROLL_HEIGHT, x_end-x_start, SCROLL_HEIGHT)
        cr.fill()
        cr.set_source_rgb(*SCROLL_HEAD_COLOR)
        cr.rectangle(x_start, height-SCROLL_HEIGHT, SCROLL_HEAD_WIDTH, SCROLL_HEIGHT)
        cr.fill()
        cr.rectangle(x_end-SCROLL_HEAD_WIDTH, height-SCROLL_HEIGHT, SCROLL_HEAD_WIDTH, SCROLL_HEIGHT)
        cr.fill()

        #Draw cursor in the scrollbar
        cr.set_line_width(1.0)
        cr.set_source_rgb(*CURSOR_COLOR)
        x = int(float(self.cursor) / self.length * width)
        cr.move_to(x, height-SCROLL_HEIGHT)
        cr.line_to(x, height)
        cr.stroke()

        #Draw scrollbar outline
        cr.set_source_rgb(0.0,0,0)
        cr.set_line_width(2.0)
        cr.rectangle(0, height-SCROLL_HEIGHT, width, SCROLL_HEIGHT)
        cr.stroke()

        #Cursor text
        cr.set_source_rgb(0.3,0.3,0.3)
        cr.move_to(2, height-SCROLL_HEIGHT - 5)
        cr.show_text("Frame: %d" % self.cursor)
