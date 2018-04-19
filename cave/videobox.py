from gi.repository import Gtk, Gdk, Pango, PangoCairo, GdkPixbuf, GLib
import cairo
import sys
import os
import numpy
from threading import Thread, Condition, Event, Lock
from time import sleep, time

try:
    import cv2
except:
    print "python-opencv is required for video access. Please install."

from libcave.video import Video
from libcave.cameralink import CameraLink
from libcave.tags.registered_tags import get_class_from_tagtype
from libcave.videoutils import verify_video

from misc.log import with_logging, supress_output

MAX_FPS = 30 #max FPS to attempt display

@with_logging
class VideoManagerThread(Thread):
    def __init__(self, callback):
        Thread.__init__(self)
        self.next_frame = None
        self.camlink = None
        self.callback = callback
        self.video = None
        self.c = Condition()
        self.kill = Event()
        self.export_to_vision = False
        self.enable_display = True
        self.width = 640
        self.height = 480
        self.limit_speed = True #Disable speed limiting only in testing
        self.grab_lock = Lock() #Preview manager may be grabbing images concurrently
        self.last_frame = None #Last frame number captured

        self.new_link = None

        self.start()

    def new_camlink(self, name, height, width, nchannels):
        with self.c:
            self.new_link = (name, height, width, nchannels)
            self.c.notify()
            #self.camlink = CameraLink(name, height=height, width=width, nChannels=nchannels)

    def set_cap(self, cap, video):
        self.log.info("Capture source changed")
        with self.c:
            self.cap = cap
            self.video = video
            self.last_frame = None

    def request_frame(self, frame):
        with self.c:
            self.next_frame = frame
            self.c.notify()

    def destroy(self):
        with self.c:
            self.kill.set()
            self.c.notify()

    # Grabs a frame from the currently loaded video
    # None if image grab failed, otherwise a tuple is returned:
    # img: raw opencv image
    # frame: cairo drawing imagesurface data (can be None if display disabled (testing))
    # width: width of image returned
    # height: height of image returned
    def grab_frame(self, frame_number, resize=None):
        with self.grab_lock:
            #Grab frame twice to avoid internal buffering (only seems to be a problem on some computers)
            # UPDATE: this doesn't seem necessary anymore; verify
            #self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_number)
            #self.cap.grab()

            #Seek to correct frame
            if self.last_frame is None or frame_number != self.last_frame + 1:
                if frame_number < 0: #DEBUG DEBUG DEBUG
                    self.log.error("Frame number was %d" % frame_number)
                else:
                    self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_number)

            success, img = self.cap.read()
            self.last_frame = frame_number

            if not success or img is None:
                #failure
                self.log.error("Failed to grab frame %d" % frame_number)
                return None
            else:
                if resize is not None:
                    ow = len(img[0]) #input frame width
                    oh = len(img) #input frame height
                    w,h = resize
                    if w is None:
                        assert(h)
                        w = int(float(h) * ow / oh)
                    if h is None:
                        assert(w)
                        h = int(float(w) * oh / ow)
                    img = cv2.resize(img, (w,h))

                width = len(img[0])
                height = len(img)
                frame = None

                if self.enable_display:
                    #cairo requires a 4th channel; add one
                    z = numpy.zeros((height, width, 1), dtype=numpy.uint8)
                    aimg = numpy.concatenate((img, z), 2).flatten()

                    frame = cairo.ImageSurface.create_for_data(aimg, cairo.FORMAT_RGB24,
                        width, height, width*4)

                return (img, frame, width, height)

    def cleanup(self):
        if self.camlink is not None:
            self.camlink.cleanup()

    def run(self):
        while True:
            with self.c:
                if self.kill.is_set():
                    break

                quit = False
                while self.next_frame is None:
                    self.c.wait()
                    if self.kill.is_set():
                        quit = True
                        break

                # We want to do all CameraLink calls from the same thread.
                if self.new_link is not None:
                    self.cleanup()
                    name, height, width, nchannels = self.new_link
                    self.camlink = CameraLink(name, height=height, width=width, nChannels=nchannels)
                    self.new_link = None

                if quit:
                    break

                frame_to_process = self.next_frame
                self.next_frame = None

            t1 = time()

            image_result = self.grab_frame(frame_to_process)

            if image_result is None:
                #Should never be null unless something is wrong with the filesystem
                self.log.error("Unable to read frame %d from file!" % frame_to_process)
            else:
                img, self.frame, self.width, self.height = image_result

                if self.frame is not None:
                    Gdk.threads_enter()
                    self.callback(self.frame)
                    Gdk.threads_leave()

                if self.export_to_vision:
                    self.camlink.send_image(img)

            if self.limit_speed:
                dt = time() - t1
                self.kill.wait(min(0, 1.0 / MAX_FPS - dt)) #sleep

        self.cleanup()

@with_logging
class VideoBox(Gtk.DrawingArea):
    """
    Box for displaying videos
    """
    def __init__ (self, parent):
        Gtk.DrawingArea.__init__(self)
        self.parent = parent
        self.connect('draw', self._do_expose)
        self.set_events(Gdk.EventMask.BUTTON_PRESS_MASK | Gdk.EventMask.BUTTON_MOTION_MASK | Gdk.EventMask.POINTER_MOTION_MASK | Gdk.EventMask.LEAVE_NOTIFY_MASK | Gdk.EventMask.ENTER_NOTIFY_MASK | Gdk.EventMask.SCROLL_MASK)
        self.connect("button-press-event", self._click) #mouse button is pressed (either left or right)
        self.connect("motion-notify-event", self._hover) #mouse is held down and in motion
        self.connect("enter-notify-event", self._enter)
        self.connect("leave-notify-event", self._exit)
        self.connect("key-press-event", self._keypress)
        self.connect("scroll-event", self._scroll)
        self.set_size_request(320, 240)

        self.frame = None
        self.video = None

        self.width = 640
        self.height = 480
        self.length = 100

        self.cap_source_cache = {}

        self.tag_type_instance = None

    def get_frame_xy(self, sx, sy):
        allocation = self.get_allocation()
        frame_width = allocation.width
        frame_height = allocation.height

        #Video origin
        vid_x_o = frame_width / 2 - self.width / 2
        vid_y_o = frame_height / 2 - self.height / 2

        return int(sx - vid_x_o), int(sy - vid_y_o)

    def _click(self, area, event):
        if self.tag_type_instance is not None:
            x,y = self.get_frame_xy(event.x, event.y)
            if self.tag_type_instance.click(area, event, x, y):
                #Increment the played frame
                self.parent.increment_frame(1)
            else:
                self.queue_draw()

    def _hover(self, area, event):
        if self.tag_type_instance is not None:
            x,y = self.get_frame_xy(event.x, event.y)
            self.tag_type_instance.hover(area, event, x, y)
            self.queue_draw()

    def _enter(self, area, event):
        if self.tag_type_instance is not None:
            x,y = self.get_frame_xy(event.x, event.y)
            self.tag_type_instance.enter(area, event, x, y)

    def _exit(self, area, event):
        if self.tag_type_instance is not None:
            x,y = self.get_frame_xy(event.x, event.y)
            self.tag_type_instance.exit(area, event, x, y)
            self.queue_draw()

    def _scroll(self, area, event):
        if self.tag_type_instance is not None:
            x,y = self.get_frame_xy(event.x, event.y)
            self.tag_type_instance.scroll(area, event, x, y)
            self.queue_draw()

    def _keypress(self, area, event):
        if self.tag_type_instance is not None:
            self.tag_type_instance.keypress(area, event)
            self.queue_draw()

    def export_to_vision(self, val):
        self.vmt.export_to_vision = val
        if val:
            self.log.info("Now exporting frames to vision")
        else:
            self.log.info("Stopping export of frames to vision")

    def enable_display(self, val):
        self.vmt.enable_display = val

    #Start the rendering thread
    def start_thread(self):
        self.vmt = VideoManagerThread(self._new_frame_arrived)

    #Callback from the rendering thread
    def _new_frame_arrived(self, frame):
        self.frame = frame
        self.width = self.vmt.width
        self.height = self.vmt.height
        self.queue_draw()

    #Kill the rendering thread
    def kill_thread(self):
        if self.vmt is not None:
            self.vmt.destroy()

    #Sets a tag for use here
    def set_tag(self, tag):
        if tag is None:
            self.tag_type_instance = None
        else:
            TagTypeClass = get_class_from_tagtype(tag.tag_type)
            self.tag_type_instance = TagTypeClass(tag, lambda : self.parent.timeline.cursor)

    #Loads a new video for playback
    #@supress_output
    def load_video(self, video):
        if video is None:
            return

        filename = Video.db.get_absolute_path(video.video_path)

        self.log.info("Load filename %s" % filename)

        self.video = video

        #opencv seems to be doing some indexing or something as it performs seeks
        #if video not in self.cap_source_cache:
        # This causes problems when multiple databases are loaded.
        #cap = cv2.VideoCapture(filename)
        #else:
        #    self.log.info("Loaded capture source from cache")
        #    self.cap = self.cap_source_cache[video]
        cap, self.width, self.height, self.length, self.nchannels = \
                verify_video(filename)
        self.cap_source_cache[video] = cap

        self.vmt.new_camlink(video.linked_camera, self.height, self.width, self.nchannels)

        self.vmt.set_cap(cap, video)
        self.vmt.request_frame(0)

    #Set the frame displayed
    def set_frame(self, frame):
        frame = max(0, min(self.length, frame)) #bounds check
        self.vmt.request_frame(frame)

    #Carries out on-screen drawing when queue_draw is called
    def _do_expose(self, widget, cr):

        # Get the size of the area that cairo has allocated for our drawing
        allocation = self.get_allocation()
        frame_width = allocation.width
        frame_height = allocation.height

        if self.tag_type_instance is None:
            scale_factor = min(float(frame_height) / self.height,
                               float(frame_width) / self.width)
        else:
            scale_factor = 1.0

        #Video origin
        vid_x_o = (frame_width / 2 / scale_factor - self.width / 2)
        vid_y_o = (frame_height / 2 / scale_factor - self.height / 2)

        if self.frame is None:
            # Nothing to draw!
            return

        #Draw the video frame
        cr.scale(scale_factor, scale_factor)
        cr.set_source_surface(self.frame, vid_x_o, vid_y_o)
        cr.paint()

        #Draw tag-related components
        if self.tag_type_instance is not None:
            self.tag_type_instance.draw(widget, cr, vid_x_o, vid_y_o)

            # Warn that auto-scale was disabled.
            cr.set_source_rgb(0.7,0,0)
            cr.move_to(0, frame_height - 5)
            # This is mostly because I'm too lazy right now to implement
            # the rescaling logic needed for tagging.
            cr.show_text("Video auto-scaling disabled in tagging mode.")
