#include "lcdwriter.h"

extern "C" {
#include <mpsse.h>
}

struct mpsse_context *spi = NULL;

// Connect to the LCD at the following baud
// Empirically set as fast as possible while
// avoiding transmission errors
#define LCD_SPEED 14e6
#define LCD_VID 0x0403
#define LCD_PID 0x6014

LcdWriter::LcdWriter() {
    spi = Open(LCD_VID, LCD_PID, SPI0, LCD_SPEED, MSB, IFACE_A, NULL, NULL);
    if (spi == NULL || spi->open == 0) {
        cerr << "Could not connect to LCD" << endl;
        throw LcdWriterOpenException();
    }
    // Reserve space for a full frame allocation
    // with safety margin of 2x
    spidata.reserve(320 * 240 * 4 * 2);
}

LcdWriter::~LcdWriter() {
    Close(spi);
}

void LcdWriter::BusWrite(uint8_t data_high, uint8_t data_low, uint8_t rd, uint8_t wr, uint8_t dc) {
    spidata.push_back(0x00); // Dummy byte
    spidata.push_back((rd & 0x01) | ((wr & 0x01) << 1) | ((dc & 0x01) << 4)); // Control pins
    spidata.push_back(data_high); // D15 - D8
    spidata.push_back(data_low); // D7 - D0
}

void LcdWriter::ComWrite(uint8_t data) {
    BusWrite(0x00, data, 1 /* rd = no read */, 0 /* wr = write */, 0 /*dc = command */); 
}

void LcdWriter::DataWrite(uint8_t data_high, uint8_t data_low) {
    BusWrite(data_high, data_low, 1 /* rd = no read */, 0 /* wr = write */, 1 /*dc = data */);
}

void LcdWriter::ComDataWrite(uint8_t com, uint16_t data) {
    ComWrite(com);
    DataWrite(data >> 8, data);
}

void LcdWriter::SendAll() {
    char* data = (char*) &spidata[0];
    Start(spi);
    Write(spi, data, spidata.size());
    Stop(spi);
    spidata.clear();
}
