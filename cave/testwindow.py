import os, sys
from time import time

from gi.repository import Gtk, Gdk, GLib
from misc.log import with_logging

from cave.libcave.video import Video

from cave.libcave.testexecutor import TestExecutor
from cave.libcave.trainexecutor import TrainExecutor
from cave.libcave.splitvideos import SplitVideos

from cave.libcave.registered_elements import get_registered_elements, \
                                             get_registered_elements_implementing
from cave.libcave.util import TimedHysteresis, populate_combo_box

try:
    import pygame
    pygame_present = True
    pygame.init()
except ImportError:
    print("Warning: pygame is not installed, there will be no sound!")
    pygame_present = False

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0])))

@with_logging
class TestWindow:
    """
    Dialog for launching a test
    """

    def _disable_buttons(self):
        self.builder.get_object("startButton").set_sensitive(False)
        self.builder.get_object("trainButton").set_sensitive(False)
        self.builder.get_object("abortButton").set_sensitive(True)

    def start_click(self, object, data=None):
        self._disable_buttons()
        self.t = TestExecutor(self.parent, self.status_callback, self.skip_value, abort_callback=self.timeout_callback, finish_callback=self.test_finish_callback)
        self.testing = True

    def starttrain_click(self, object, data=None):
        self._disable_buttons()
        self.t = TrainExecutor(self.parent, self.status_callback, self.skip_value, abort_callback=self.timeout_callback)
        self.testing = True

    def start_split_video_click(self, object, data=None):
        self._disable_buttons()
        self.t = SplitVideos(self.parent, self.status_callback, self.skip_value, abort_callback=self.timeout_callback)
        self.testing = True

    def timeout_callback(self):
        Gdk.threads_enter()
        self.abort_click(None)
        md = Gtk.MessageDialog(self.parent.window, Gtk.DialogFlags.DESTROY_WITH_PARENT, Gtk.MessageType.ERROR,
                Gtk.ButtonsType.CLOSE, "Test timeout!")
        md.format_secondary_text("Did not receive response from vision daemon. Perhaps it is not running?\n" +
                                 "Make sure the vision daemon is started with \"auv-visiond cave_test\"\n\n" +
                                 "Also, verify that modules for elements being tested are included in the " +
                                 "\"cave_test\" configuration file.\n")
        md.run()
        md.destroy()
        Gdk.threads_leave()

    def abort_click(self, object, data=None):
        if self.t is not None:
            self.t.abort()
            self.testing = False
            self.window_destroy(None)

    def window_destroy(self, object, data=None):
        if self.testing:
            self.t.abort()
        self.log.debug("Window closed")
        self.window.destroy()
        if not self.testing:
            self.parent.video_tree.set_sensitive(True)

    def spin_button_change(self, object, data=None):
        val = self.spin_button.get_value()
        if val is None:
            self.spin_button.set_value(1)
        else:
            self.skip_value = int(val)
            self.populate_info()

    def mission_element_changed(self, object, data=None):
        me = self.elements[self.mission_element_combo.get_active()]
        self.parent.video_tree_manager.exclusively_enable_by_element(me)
        self.populate_info()

    def test_finish_callback(self, frames_correct):
        self.frames_correct = frames_correct

    def status_callback(self, frames_done):
        Gdk.threads_enter()
        self.percent_text.set_text("%.1f%%" % (float(frames_done) / self.total_frames * 100))
        self.progress_bar.set_fraction(float(frames_done) / self.total_frames)

        if self.last_frame_time is not None:
            fps = 1.0 / (time() - self.last_frame_time)
            self.fps_hysteresis.update(fps)
            fps = self.fps_hysteresis.get()
            if fps is not None:
                self.test_info_text.set_text("Testing: %.1f fps" % fps)
        self.last_frame_time = time()

        if frames_done == self.total_frames:
            #Done with test
            if pygame_present:
                try:
                    pygame.mixer.Sound(os.path.join(__location__,"resources/ding.wav")).play()
                except pygame.error as e:
                    self.log.warning("Playing the ding failed... " + str(e))
            self.testing = False
            self.info_text.set_text("")
            md = Gtk.MessageDialog(self.parent.window, \
                                   Gtk.DialogFlags.DESTROY_WITH_PARENT, \
                                   Gtk.MessageType.INFO, \
                                   Gtk.ButtonsType.CLOSE, "Test results!")
            md.format_secondary_text("Correct frames: %d/%d\n" % (self.frames_correct, frames_done) +
                                     "Accuracy      : %f\n" % (self.frames_correct / float(frames_done)))
            md.run()
            md.destroy()

            self.window_destroy(None) #Close when done testing (TODO: good behavior?)
        Gdk.threads_leave()

    def populate_info(self):
        self.info_text = self.builder.get_object("infoText")

        self.total_frames = 0

        for vid in Video.get_all():
            tgs = vid.get_tags()
            for tag in tgs:
                if tag.active:
                    frame_list = list(tag.get_frame_list())[::self.skip_value]
                    self.total_frames += len(frame_list)

        self.info_text.set_text("Frames to process: %d" % self.total_frames)

        start_enabled = self.total_frames > 0
        self.builder.get_object("startButton").set_sensitive(start_enabled)
        self.builder.get_object("trainButton").set_sensitive(start_enabled)


    def __init__(self, parent):
        self.gladefile = os.path.join(__location__, "gui/testwindow.glade")
        self.builder = Gtk.Builder()
        self.builder.add_from_file(self.gladefile)
        self.parent = parent
        self.t = None
        self.last_frame_time = None
        self.fps_hysteresis = TimedHysteresis(1.0) #1 second hysteresis

        # Automatically connect signals to functions defined above
        self.builder.connect_signals(self)

        self.mission_element_combo = self.builder.get_object("missionElementCombo")
        self.tag_type_combo = self.builder.get_object("tagTypeCombo")
        self.tag_name_entry = self.builder.get_object("tagNameEntry")

        self.progress_bar = self.builder.get_object("progressBar")
        self.percent_text = self.builder.get_object("percentText")
        self.test_info_text = self.builder.get_object("testInfoText")

        self.spin_button = self.builder.get_object("spinButton")
        self.spin_button.set_numeric(True)
        adjustment = Gtk.Adjustment(value=1, lower=1, upper=50, step_incr=1, page_incr=0, page_size=0)
        self.spin_button.set_adjustment(adjustment)
        self.spin_button.set_value(1)

        self.skip_value = 1

        #Add information about the test about to be run
        self.populate_info()

        #Populate mission element dropdown
        self.elements = get_registered_elements().keys()
        self.elements = sorted(self.elements)
        populate_combo_box(self.mission_element_combo, self.elements)

        #Get the main window
        self.window = self.builder.get_object("testWindow")
        self.window.set_type_hint(Gdk.WindowTypeHint.DIALOG)
        self.window.show()

        #Disable the trees to prevent modifications
        self.parent.video_tree.set_sensitive(False)

        self.testing = False

        self.log.debug("Window created")

