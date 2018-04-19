#ifndef LCD_H_
#define LCD_H_

#include <cairo/cairo.h>

#include "lcdwriter.h"

/*
 * Provides access to the LCD
 * The LCD supports writing a cairo surface
 */
class Lcd {
    private:
        void delay(int i);
        void SetXY(int x1, int y1, int x2, int y2);
        void WriteData(unsigned char* data);

        bool initialized = false;
        LcdWriter writer; // Provides low-level communication with the screen

    public:
        // LCD must be initialized before writing to the screen
        void Init();

        // Fill the screen with black (clear)
        void Clear();

        // Writes a cairo surface to the screen
        // surface must be 320x240 and in RGB16_565 format
        void WriteSurface(cairo_surface_t* surface);
};

// Wrapper for ctypes (python interface)
extern "C" {
    Lcd* Lcd_new() { return new Lcd(); }
    void Lcd_destroy(Lcd* obj) { delete obj; }
    void Lcd_init(Lcd* obj) { obj->Init(); }
    void Lcd_clear(Lcd* obj) { obj->Clear(); }
    void Lcd_writesurface(Lcd* obj, char* surface) { obj->WriteSurface((cairo_surface_t*) surface); }
}

#endif // LCD_H_
