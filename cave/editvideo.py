import os
import sys
from gi.repository import Gtk
from misc.log import with_logging
from addvideo import AddVideo


__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0])))

@with_logging
class EditVideo(AddVideo):
    '''
    Dialog for editing a video's meta data and other identifiers
    '''

    def __init__(self, callback, edit):
        AddVideo.__init__(self, callback)

        self.window.set_title("Video Edit")
        label = self.builder.get_object("label1")
        label.set_text(label.get_text().replace("Add", "Edit").replace("add", "edit"))

        self.video = edit

        #Set initial values
        camera_link_ind = [row[0] for row in self.camera_link.get_model()].index(edit.linked_camera)
        self.camera_link.set_active(camera_link_ind)

        self.video_path.set_filename(edit.video_path)
        if edit.log_path:
            self.log_path.set_filename(edit.log_path)

        self.video_name_entry.set_text(edit.name)
        self.meta_entry.set_text(edit.meta)

