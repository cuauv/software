import cairo
import ctypes

from auv_python_helpers import load_library

# XXX: this binding is missing from pycairo?
# If cairo ever changes their header, this could break!
FORMAT_RGB16_565 = 4

class Lcd:
    """
    Ctypes wrapper class for Lcd @ lcd.h
    """

    WIDTH = 320
    HEIGHT = 240

    def __init__(self):
        self.lib = load_library("liblcd.so")
        self.obj = self.lib.Lcd_new()

    def __del__(self):
        try:
            self.lib.Lcd_destroy(self.obj)
        except:
            pass

    def init(self):
        self.lib.Lcd_init(self.obj)

    def clear(self):
        self.lib.Lcd_clear(self.obj)

    def write_surface(self, surface):
        assert(type(surface) == cairo.ImageSurface)
        # XXX: Mad hax going on here. Let me explain.
        # We need a pointer to the underlying cairo surface. Since pycairo wraps
        # the cairo C library, and we know that the Pycairo_ImageSurface object's
        # first element is a pointer to the C struct, we can get the address by
        # taking the python object's address and skipping its PyObject_HEAD.
        # 
        # In other words, this is wildly dependant on the internal implementation
        # of pycairo and may break in the future.
        self.lib.Lcd_writesurface(self.obj, ctypes.c_char_p.from_address(id(surface) + object.__basicsize__))

    @classmethod
    def new_surface(cls):
        surface = cairo.ImageSurface(FORMAT_RGB16_565, cls.WIDTH, cls.HEIGHT)
        cr = cairo.Context(surface)
        return surface, cr
