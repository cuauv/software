
from gi.repository import Gtk
import os

class FileTypes:
    CDB     = 1
    AVI     = 2
    SHMLOG  = 3
    ALL     = 4

filetypes = {FileTypes.CDB: ("Cave Database Files (*.cdb)", "*.cdb"),
             FileTypes.AVI: ("AVI Video File (*.avi)", "*.avi"),
             FileTypes.SHMLOG: ("Shared Memory Log File (*.shmlog)", "*.shmlog"),
             FileTypes.ALL: ("All Files", "*")}


class FilePicker:
    """
    Wrapper for a file chooser
    """

    @classmethod
    def new(cls, **args):
        return cls.__prompt(Gtk.FileChooserAction.SAVE, Gtk.STOCK_SAVE, **args)

    @classmethod
    def open(cls, **args):
        return cls.__prompt(Gtk.FileChooserAction.OPEN, Gtk.STOCK_OPEN, **args)

    @classmethod
    def __prompt(cls, opt1, opt2, title="", default_filename=None, type_list=[]):
        #Generic file choose prompt for open / new
        dialog = Gtk.FileChooserDialog(title,
                                       None,
                                       opt1,
                                       (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
                                        opt2, Gtk.ResponseType.OK))
        dialog.set_default_response(Gtk.ResponseType.OK)

        dialog.set_current_folder(os.getcwd())

        if opt2 == Gtk.STOCK_SAVE and default_filename is not None:
            dialog.set_current_name(default_filename)

        type_list.append(FileTypes.ALL)

        for type_enum in set(type_list):
            (type_string, type_ext) = filetypes[type_enum]                
            filter = Gtk.FileFilter()
            filter.set_name(type_string)
            filter.add_pattern(type_ext)
            dialog.add_filter(filter)

        ret = None
        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            ret = dialog.get_filename()
        dialog.destroy()
        return ret


