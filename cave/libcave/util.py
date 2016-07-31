from time import time
from gi.repository import Gtk

# General use utility functions / classes

# Computes average value of values within the past "history_time" seconds
class TimedHysteresis:
    def __init__(self, history_time):
        self.history_time = history_time
        self.values = []
   
    def _clear_old_values(self):
        def time_is_valid(e):
            (val, t) = e
            return t >= (time() - self.history_time)
        self.values = filter(time_is_valid, self.values)

    def update(self, new_value): 
        self.values.append((new_value, time()))

    def get(self):
        self._clear_old_values()
        if len(self.values) == 0:
            return None
        else:
            vals = [y[0] for y in self.values]
            return float(sum(vals)) / len(vals)

def populate_combo_box(combo, items):
    combo.clear()
    liststore = Gtk.ListStore(str)
    for e in items:
        liststore.append([e])
    combo.set_model(liststore)
    cell = Gtk.CellRendererText()
    if len(items) == 1:
        combo.set_active(0)
    combo.pack_start(cell, True)
    combo.add_attribute(cell, "text", 0)








