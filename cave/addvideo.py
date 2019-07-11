
import sys
import os

from gi.repository import Gtk, Gdk
from misc.log import with_logging
from cave.libcave.cameralink import camera_map
from cave.meta import MetaParser

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0])))

@with_logging
class AddVideo:
    """
    Dialog for adding a video to the database
    """

    last_path = None # Stores the path of the last opened video

    def validate(self):
        #Validates form input; also sets variables for use higher up
        warning_label = self.builder.get_object("warningLabel")

        self.video_filename = self.video_path.get_filename()
        if not (self.video_filename is not None and os.path.isfile(self.video_filename)):
            warning_label.set_text("Invalid Video File")
            return False

        self.log_filename = self.log_path.get_filename()
        if self.log_filename and not os.path.isfile(self.log_filename):
            warning_label.set_text("Invalid Log File")
            return False

        self.video_name = self.video_name_entry.get_text()
        if self.video_name == "":
            warning_label.set_text("Invalid video name")
            return False

        if self.camera_link.get_active() == -1:
            warning_label.set_text("Camera link invalid")
            return False
        else:
            #TODO: make this not hacky
            self.linked_camera = tuple(camera_map.keys())[self.camera_link.get_active()]

        self.meta = MetaParser(self.meta_entry.get_text()).parse()

        return True


    def cancel_click(self, object, data=None):
        self.window.destroy()

    def ok_click(self, object, data=None):
        if self.validate():
            self.window.destroy()
            AddVideo.last_path = os.path.dirname(self.video_filename)
            self.log.info("Valid parameters; executing callback")
            self.callback(self)
        else:
            self.log.warning("Invalid parameters specified")
            warning_box = self.builder.get_object("warningBox")
            warning_box.set_visible(True)

    def window_destroy(self, object, data=None):
        self.log.debug("Window closed")
        self.window.destroy()

    def __init__(self, callback, default_folder=None):
        self.gladefile = os.path.join(__location__, "gui/addvideo.glade")
        self.builder = Gtk.Builder()
        self.builder.add_from_file(self.gladefile)

        # Automatically connect signals to functions defined above
        self.builder.connect_signals(self)

        #Form Elements
        self.camera_link = self.builder.get_object("cameraLinkCombo")
        self.video_path = self.builder.get_object("videoPathChooser")
        self.log_path = self.builder.get_object("logPathChooser")
        self.video_name_entry = self.builder.get_object("videoNameEntry")
        self.meta_entry = self.builder.get_object("metaEntry")

        #If a previous video is added, open the folder containing that video instead
        if AddVideo.last_path is not None:
            self.video_path.set_current_folder(AddVideo.last_path)
            self.log_path.set_current_folder(AddVideo.last_path)
        #Set default folders for the file-buttons
        elif default_folder is not None:
            self.video_path.set_current_folder(default_folder)
            self.log_path.set_current_folder(default_folder)

        #Populate camera link dropdown
        liststore = Gtk.ListStore(str)
        for cam in camera_map.keys():
            liststore.append([cam])
        self.camera_link.set_model(liststore)
        cell = Gtk.CellRendererText()
        self.camera_link.pack_start(cell, True)
        self.camera_link.add_attribute(cell, "text", 0)

        #Get the main window
        self.window = self.builder.get_object("addVideoWindow")
        self.window.set_type_hint(Gdk.WindowTypeHint.DIALOG)
        self.window.show()

        #Link callback
        self.callback = callback

        self.log.debug("Window created")

