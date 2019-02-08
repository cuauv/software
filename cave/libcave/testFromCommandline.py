#! /usr/bin/env python2
import os, sys
from time import time

from misc.log import with_logging
from cave.libcave.database import Database
from cave.libcave.video import Video
import cave.libcave.tags.notvisible, cave.libcave.tags.centerpointvisible, cave.libcave.tags.centerpointvisibledirection
from cave.libcave.testexecutor import TestExecutor
from cave.libcave.trainexecutor import TrainExecutor
from cave.libcave.splitvideos import SplitVideos

from cave.libcave.registered_elements import get_registered_elements, \
                                get_registered_elements_implementing
from cave.libcave.util import TimedHysteresis, populate_combo_box

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

def cli_progress_test(end_val, bar_length=20):
    for i in xrange(0, end_val):
        percent = float(i) / end_val
        hashes = '#' * int(round(percent * bar_length))
        spaces = ' ' * (bar_length - len(hashes))
        sys.stdout.write("\rPercent: [{0}] {1}%".format(hashes + spaces, int(round(percent * 100))))
        sys.stdout.flush()

@with_logging
class TestFromCommandline:
    def startTest(self):
        self.t = TestExecutor(None, self.status_callback, self.skip_value, abort_callback=self.timeout_callback, finish_callback=self.test_finish_callback)
        self.testing = True

    def populate_info(self):
        self.total_frames = 0
        for vid in Video.get_all():
            tgs = vid.get_tags()
            for tag in tgs:
                if tag.active:
                    frame_list = tag.get_frame_list()[::self.skip_value]
                    self.total_frames += len(frame_list)

        print("Frames to process: %d" % self.total_frames)

    #Load the database from a given path
    def load_db(self, path):
        self.db = Database(path)
        if self.db is not None and self.db.error:
            self.log.error("Failed to load database %s" % path)
            return
        if self.db is not None and path is not None:
            path = os.path.abspath(path)
            #self.db_label = self.builder.get_object("databaseLabel")
            #self.db_label.set_text(os.path.basename(path))
            #self.window.set_title("CAVE -- %s" % os.path.basename(path))
            #self.statusbar.display("Opened database %s" % path, 3)
            #self.log("Opened database %s" % os.path.basename(path))
           # self.video_tree_manager.redraw()
            
            #Enable all operations

    def __init__(self,db): # parent was parentwindow
       # self.parent = parent
        self.t = None
        self.last_frame_time = None
        self.fps_hysteresis = TimedHysteresis(1.0) #1 second hysteresis
        self.load_db(db)
        self.skip_value = 1

        #add info about test to be run
        self.populate_info()
        self.elements = get_registered_elements().keys()

        #Disable the trees to prevent modifications
        #self.parent.video_tree.set_sensitive(False)

        self.testing = False

        print("Test from cmdline created")


    def startTrain(self, object,data=None):
        self.t = TrainExecutor(self.parent, self.status_callback, self.skip_value, abort_callback=self.timeout_callback)
        self.testing = True

    def start_split_video_click(self, object, data=None):
        self.t = SplitVideos(self.parent, self.status_callback, self.skip_value, abort_callback=self.timeout_callback)
        self.testing = True

    def timeout_callback(self):
        print("test timeout!")
        print("Did not receive response from vision daemon. Perhaps it is not running?\n" +
                                 "Make sure the vision daemon is started with \"auv-visiond cave_test\"\n\n" +
                                 "Also, verify that modules for elements being tested are included in the " +
                                 "\"cave_test\" configuration file.\n")

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
        #Gdk.threads_enter()
        # XXX make a new commandline progressbar
        cli_progress_test(float(frames_done) / self.total_frames * 100)
        #self.percent_text.set_text("%.1f%%" % (float(frames_done) / self.total_frames * 100))
        #self.progress_bar.set_fraction(float(frames_done) / self.total_frames)

        if self.last_frame_time is not None:
            fps = 1.0 / (time() - self.last_frame_time)
            self.fps_hysteresis.update(fps)
            fps = self.fps_hysteresis.get()
            if fps is not None:
                self.test_info_text.set_text("Testing: %.1f fps" % fps)
        self.last_frame_time = time()

        if frames_done == self.total_frames:
            #Done with test
            self.testing = False
            self.info_text.set_text("")
            print("Correct frames: %d/%d\n" % (self.frames_correct, frames_done) +
                                     "Accuracy      : %f\n" % (self.frames_correct / float(frames_done)))


 


