#include "lcd.h"

#include <assert.h>
#include <unistd.h>

// Screen dimensions
#define LCD_WIDTH 320
#define LCD_HEIGHT 240

void Lcd::delay(int i) {
    usleep(i * 1000);
    writer.SendAll();
}

void Lcd::Init() {
    // LCD startup command sequence taken from 
    // microcontroller driver
    writer.ComDataWrite(0x0000,0x0001); delay(1);
    writer.ComDataWrite(0x0003,0xA8A4); delay(1);
    writer.ComDataWrite(0x000C,0x0000); delay(1);   
    writer.ComDataWrite(0x000D,0x080C); delay(1);   
    writer.ComDataWrite(0x000E,0x2B00); delay(1);   
    writer.ComDataWrite(0x001E,0x00B7); delay(1);   
    writer.ComDataWrite(0x0001,0x2B3F); delay(1);
    writer.ComDataWrite(0x0002,0x0600); delay(1);
    writer.ComDataWrite(0x0010,0x0000); delay(1);
    writer.ComDataWrite(0x0011,0x6070); delay(1);
    writer.ComDataWrite(0x0005,0x0000); delay(1);
    writer.ComDataWrite(0x0006,0x0000); delay(1);
    writer.ComDataWrite(0x0016,0xEF1C); delay(1);
    writer.ComDataWrite(0x0017,0x0003); delay(1);
    writer.ComDataWrite(0x0007,0x0233); delay(1);
    writer.ComDataWrite(0x000B,0x0000); delay(1);
    writer.ComDataWrite(0x000F,0x0000); delay(1);
    writer.ComDataWrite(0x0041,0x0000); delay(1);
    writer.ComDataWrite(0x0042,0x0000); delay(1);
    writer.ComDataWrite(0x0048,0x0000); delay(1);
    writer.ComDataWrite(0x0049,0x013F); delay(1);
    writer.ComDataWrite(0x004A,0x0000); delay(1);
    writer.ComDataWrite(0x004B,0x0000); delay(1);
    writer.ComDataWrite(0x0044,0xEF00); delay(1);
    writer.ComDataWrite(0x0045,0x0000); delay(1);
    writer.ComDataWrite(0x0046,0x013F); delay(1);
    writer.ComDataWrite(0x0030,0x0707); delay(1);
    writer.ComDataWrite(0x0031,0x0204); delay(1);
    writer.ComDataWrite(0x0032,0x0204); delay(1);
    writer.ComDataWrite(0x0033,0x0502); delay(1);
    writer.ComDataWrite(0x0034,0x0507); delay(1);
    writer.ComDataWrite(0x0035,0x0204); delay(1);
    writer.ComDataWrite(0x0036,0x0204); delay(1);
    writer.ComDataWrite(0x0037,0x0502); delay(1);
    writer.ComDataWrite(0x003A,0x0302); delay(1);
    writer.ComDataWrite(0x003B,0x0302); delay(1);
    writer.ComDataWrite(0x0023,0x0000); delay(1);
    writer.ComDataWrite(0x0024,0x0000); delay(1);
    writer.ComDataWrite(0x0025,0x8000); delay(1);
    writer.ComDataWrite(0x004f,0);
    writer.ComDataWrite(0x004e,0);
    writer.ComWrite(0x22);
    writer.SendAll();
    initialized = true;
}

void Lcd::SetXY(int x1, int y1, int x2, int y2) {
    writer.ComDataWrite(0x0044,(y2<<8)+y1);
    writer.ComDataWrite(0x0045,x1);
    writer.ComDataWrite(0x0046,x2);
    writer.ComDataWrite(0x004e,y1);
    writer.ComDataWrite(0x004f,x1);
    writer.ComWrite(0x22);        
}

void Lcd::Clear() {
    assert(initialized);
    SetXY(0,0,LCD_WIDTH-1,LCD_HEIGHT-1);
    for (int i = 0; i < LCD_WIDTH * LCD_HEIGHT; i++) {
        writer.DataWrite(0,0);
    }
    writer.SendAll();
}

// Writes a 320x240 cairo image surface's internal data to the LCD
// handles the transform required to rotate into
// landscape view
void Lcd::WriteData(unsigned char* data) {
    int loc;
    SetXY(0,0,LCD_WIDTH-1,LCD_HEIGHT-1);
    for (int y = 0; y < LCD_WIDTH; y++) {
        for (int x = 0; x < LCD_HEIGHT; x++) {
            loc = 2 * (LCD_WIDTH * x + (LCD_WIDTH-1-y));
            writer.DataWrite(data[loc+1], data[loc]);
        }
    }
    writer.SendAll();
}

void Lcd::WriteSurface(cairo_surface_t* surface) {
    assert(initialized);
    assert(cairo_image_surface_get_width(surface) == LCD_WIDTH);
    assert(cairo_image_surface_get_height(surface) == LCD_HEIGHT);
    assert(cairo_image_surface_get_format(surface) == CAIRO_FORMAT_RGB16_565);
    cairo_surface_flush(surface);
    unsigned char* data = cairo_image_surface_get_data(surface);

    // TODO: differential writing based on areas that have changed
    WriteData(data);
}

