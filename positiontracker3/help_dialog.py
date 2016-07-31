from gi.repository import Gtk, Gdk, GLib
import user_config as conf

class HelpDialog(Gtk.Dialog):

    def __init__(self):
        b = (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL, Gtk.STOCK_OK, Gtk.ResponseType.OK)
        Gtk.Dialog.__init__(self, title="Help", buttons=b)

        self.set_default_size(150, 100)
        box = self.get_content_area()

        label = Gtk.Label("Below are the key bindings, change in user_config.py")
        box.add(label)
        bind_keys = conf.key_binds.keys()
        list.sort(bind_keys)
        for bind in bind_keys:
            box.add(Gtk.Label(str(bind) + ": " + str(conf.key_binds[bind])))
        self.show_all()
