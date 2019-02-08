from gi.repository import Gtk, Gdk, GLib
from threading import Thread, Condition, Event, Lock
from threading import Thread
from misc.log import with_logging

from time import sleep

from cave.libcave.tag import Tag
from cave.libcave.video import Video

from cave.libcave.registered_elements import get_registered_elements
from cave.libcave.tags.registered_tags import get_class_from_tagtype, get_required_functions_of_tag

@with_logging
class ExecutorTimeout(Thread):

    TIMEOUT = 4 #Waiting for vision timeout

    def __init__(self, callback):
        Thread.__init__(self)
        self.callback = callback
        self.c = Condition()
        self.daemon = True
        self.kill = False
        self.waiter = Event()
        self.start_clock = False

        self.start()

    def set(self):
        with self.c:
            self.start_clock = True
            self.c.notify()

    def clear(self):
        with self.c:
            self.waiter.set()

    def destroy(self):
        with self.c:
            self.kill = True
            self.c.notify()

    def run(self):
        while True:
            with self.c:
                while not (self.start_clock or self.kill):
                    self.c.wait()
            if self.kill:
                return
            self.start_clock = False
            self.waiter.wait(self.TIMEOUT)
            with self.c:
                if not self.waiter.is_set():
                    self.log.error("Received no response from vision daemon. Aborting!")
                    self.log.error("Perhaps the vision daemon is not started?")
                    self.callback() #execute abort callback
                self.waiter.clear()

class Executor(Thread):

    def __init__(self, parent, status_callback, skip, abort_callback=None):
        Thread.__init__(self)
        self.daemon = False #temp again
        self.parent = parent
        self.skip = skip
        self.status_callback = status_callback
        self.abort_flag = False

        self.current_melement = None
        self.l = Lock()
        self.timeout = ExecutorTimeout(abort_callback)

        self.start()

    def abort(self):
        self.log.info("Aborting the test")
        self.abort_flag = True

        with self.l:
            if self.current_melement is not None:
                self.current_melement.broadcast()

    def has_aborted(self):
        return self.abort_flag

    def prep_gui_for_test(self):
        Gdk.threads_enter()
        self.parent.video_box.enable_display(False) #Stop showing video
        self.prev_export = self.parent.video_box.vmt.export_to_vision #hacky, fix
        self.prev_video = self.parent.video_box.video
        self.parent.video_tree.set_sensitive(False)
        self.parent.video_box.vmt.limit_speed = False
        self.parent.video_box.export_to_vision(True)
        Gdk.threads_leave()

    def restore_gui_after_test(self):
        Gdk.threads_enter()
        self.parent.video_box.enable_display(True) #Show video again
        self.parent.video_box.vmt.limit_speed = True
        self.parent.video_box.export_to_vision(self.prev_export)
        self.parent.video_tree.set_sensitive(True)
        self.parent.video_box.queue_draw()
        self.parent.timeline.queue_draw()
        self.parent.video_tree_manager.redraw()
        Gdk.threads_leave()

        if self.prev_video is not None:
            sleep(1) #Sleep fixes a horrific threading bug #TODO: FIX
            Gdk.threads_enter()
            self.parent.video_box.load_video(self.prev_video)
            Gdk.threads_leave()

    def timeout_set(self, m_element):
        with self.l:
            self.current_melement = m_element
            self.timeout.set()

    def timeout_clear(self):
        with self.l:
            self.current_melement = None
            self.timeout.clear()

    # Generator returns tags to test on
    def get_frames(self, vision_enable=True):
        melements = get_registered_elements()
        frame_count = 0

        for vid in Video.get_all():
            tgs = vid.get_tags()
            if len(tgs) > 0:
                self.log.info("Loading video id %d" % vid.id)
                if(self.parent != None):
                    self.parent.video_box.load_video(vid)
                # XXX The below sleep was added to prevent a "Bus Error"
                # XXX when running a test with a lot of disabled tags - Alex S.
                # TODO FIX
                sleep(0.1)

            for tag in filter(lambda t: t.active, tgs):
                frame_list = tag.get_frame_list()[::self.skip]
                if len(frame_list) > 0:
                    #Carry out testing on this tag
                    self.log.info("Testing tag #%d (%d frames)" % (tag.id, len(frame_list)))
                    frame_list.sort()
                    tag.clear_test_results()
                    if vision_enable:
                        if not melements.has_key(tag.mission_element):
                            self.log.warning("Skipping tag %s; not a mission element." % tag.mission_element)
                            continue

                        m_element = melements[tag.mission_element]()
                        m_element.init() #Turn on vision for this mission element

                    for frame in frame_list:
                        #The test of the frame
                        if vision_enable:
                            yield frame, tag, m_element
                        else:
                            yield frame, tag

                        frame_count += 1
                        self.status_callback(frame_count)

                        if self.has_aborted():
                            break

                    if vision_enable:
                        m_element.deinit() #Turn off vision for this mission element

                    self.log.info("Waiting for module shutdown")
                    sleep(1) # We sleep here because vision will reset the enabled flag if vision is stopped and started too qucikly
                             # This happens if we immediately stop and start the same vision module
                             # TODO: Refactor to fix this (or fix this behavior in vision)
                             # Testing could potentially time out if a vision module does not shut down within
                             # this period
                if self.has_aborted():
                    break
            if self.has_aborted():
                break


    def execute(self):
        raise NotImplemented("Executor is abstract")

    def run(self):
        self.log.critical("Test started!")
        if (self.parent != None):
            self.prep_gui_for_test()

        self.execute()

        self.timeout.destroy()
        if(self.parent != None):
            self.restore_gui_after_test()
