import cairo
from screen import Screen
from time import time
import shm
from common import *
import shm

FLASH_FREQ = 2.0 #hz

class MessageScreen(Screen):
    
    def get_name(self):
        return "message"

    def draw(self, cr):

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(40)

        def get_width(s):
            (x, y, width, height, dx, dy) = cr.text_extents(s)
            return width

        def get_height(s):
            (x, y, width, height, dx, dy) = cr.text_extents(s)
            return height

        def break_lines(s):
            words = s.split(" ")
            words.reverse()

            lines = []
            current = []

            while len(words) > 0:
                if len(current) == 0:
                    current.append(words.pop())
                else:
                    new_word = words.pop()
                    if new_word == "<br>":
                        lines.append(" ".join(current))
                        current = []
                    else:
                        if get_width(" ".join(current + [new_word])) > 320:
                            # Line too long
                            lines.append(" ".join(current))
                            current = [new_word]
                        else:
                            current.append(new_word)

            if len(current) > 0:
                lines.append(" ".join(current))

            return lines

        all_lines = break_lines(shm.lcd.message.get())

        V_PAD = 5

        total_height = sum([get_height(x) for x in all_lines]) + (V_PAD * (len(all_lines) - 1))

        t = int(time() * FLASH_FREQ)
        modcolor = RED if t%2 == 0 else YELLOW
        cr.set_source_rgb(*modcolor)
        (x, y, width, height, dx, dy) = cr.text_extents("test")
        
        y = 240/2 - total_height / 2
       

        for l in all_lines:
            width = get_width(l)
            height = get_height(l)
            cr.move_to(320 / 2 - width/2, y + height/2)
            y += get_height(l) + V_PAD
            cr.show_text(l)



