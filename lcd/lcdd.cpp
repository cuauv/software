#include <iostream>

#include <cairo/cairo.h>
#include "lcd.h"

using namespace std;

/*
 * Testing drawing to the LCD using the C++
 * LCD class directly.
 *
 * DEPRECATED
 *
 * May be useful if the python
 * cairo bindings are causing trouble
 */

int main() {
    Lcd screen;
    screen.Init();

    cairo_surface_t *surface;
    cairo_t *cr;

    surface = cairo_image_surface_create(CAIRO_FORMAT_RGB16_565, 320, 240);
    cr = cairo_create(surface);

    cairo_set_source_rgb(cr, 255, 0, 0);
    cairo_select_font_face(cr, "monospace", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size(cr, 20.0);

    cairo_move_to(cr, 10.0, 50.0);

    cairo_show_text(cr, "Hello World!");
    cairo_move_to(cr, 10.0, 80.0);
    cairo_show_text(cr, "This is the LCD screen.");
    cairo_move_to(cr, 10.0, 110.0);
    cairo_show_text(cr, "So pixels. Much wow.");

    screen.WriteSurface(surface);
    
    cairo_destroy(cr);
    cairo_surface_destroy(surface);

    cout << "Done writing test image" << endl;
    return 0;
}

