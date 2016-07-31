import cairo
from screen import Screen

class ExitScreen(Screen):

    def get_name(self):
        return "exit"

    def draw(self, cr):
        cr.set_source_rgb(1.0, 0, 0)
        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(150)
        cr.move_to(10, 150)
        cr.show_text("Bye!")

        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.set_font_size(25)
        cr.move_to(15, 230)
        cr.show_text("Please wait for system halt.")

