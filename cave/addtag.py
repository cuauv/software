import sys
import os

from gi.repository import Gtk, Gdk
from misc.log import with_logging

from libcave.registered_elements import get_registered_elements, get_registered_elements_implementing
from libcave.tags.registered_tags import get_tagtype_names, get_required_functions_of_tag
from libcave.util import populate_combo_box

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0])))

@with_logging
class AddTag:
    """
    Dialog for adding a tag to the database
    """

    def validate(self):
        #Validates form input; also sets variables for use higher up
        ## GUI Part
        warning_label = self.builder.get_object("warningLabel")
        self.tag_name = self.tag_name_entry.get_text()
        if self.tag_name == "":
            warning_label.set_text("Invalid tag name")
            return False

        if self.mission_element_combo.get_active() == -1:
            warning_label.set_text("Mission Element invalid")
            return False
        else:
            self.mission_element = self.elements[self.mission_element_combo.get_active()]

        if self.tag_type_combo.get_active() == -1:
            warning_label.set_text("Tag Type invalid")
            return False
        else:
            self.tag_type = self.tag_types[self.tag_type_combo.get_active()]
        return True

    def cancel_click(self, object, data=None):
        self.window.destroy()

    def ok_click(self, object, data=None):
        if self.validate():
            self.window.destroy()
            self.log.info("Valid parameters; executing callback")
            self.callback(self)
        else:
            self.log.warning("Invalid parameters specified")
            warning_box = self.builder.get_object("warningBox")
            warning_box.set_visible(True)

    def window_destroy(self, obj, data=None):
        self.log.debug("Window closed")
        self.window.destroy()

    def mission_element_changed(self, object, data=None):
        me = self.elements[self.mission_element_combo.get_active()]
        types = []
        for t in get_tagtype_names():
            req_functions = get_required_functions_of_tag(t)
            if me in get_registered_elements_implementing(req_functions):
                types.append(t)
        self.set_tag_types(types)

    def set_tag_types(self, types):
        types.sort()
        self.tag_types = types
        populate_combo_box(self.tag_type_combo, types)

    def __init__(self, callback):
        self.gladefile = os.path.join(__location__, "gui", "addtag.glade")
        self.builder = Gtk.Builder()
        self.builder.add_from_file(self.gladefile)

        # Automatically connect signals to functions defined above
        # not needed in commandline
        self.builder.connect_signals(self)

        self.mission_element_combo = self.builder.get_object("missionElementCombo")
        self.tag_type_combo = self.builder.get_object("tagTypeCombo")
        self.tag_name_entry = self.builder.get_object("tagNameEntry")

        #Populate dropdowns
        self.elements = get_registered_elements().keys()
        self.elements.sort()
        populate_combo_box(self.mission_element_combo, self.elements)
        self.set_tag_types([])

        #Get the main window
        self.window = self.builder.get_object("addTagWindow")
        self.window.set_type_hint(Gdk.WindowTypeHint.DIALOG)
        self.window.show()

        #Link callback
        self.callback = callback

        self.log.debug("Window created")

