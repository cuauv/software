#ifndef LCDWRITER_H_
#define LCDWRITER_H_

#include <iostream>
#include <stdint.h>
#include <vector>

using namespace std;

class LcdWriterOpenException {}; // Thrown if LCD cannot be opened

/*
 * Handles low-level communication with the LCD
 */
class LcdWriter {
private:
    /* 
     * Queues up data to be written to the LCD
     * All arguments correspond to pins on the SPI Interaface Board
     * @data_high: D15-D8
     * @data_low: D7-D0
     * @rd: rd line
     * @wr: wr line
     * @dc: dc line
     */
    void BusWrite(uint8_t data_high, uint8_t data_low, uint8_t rd, uint8_t wr, uint8_t dc); 

    // Data buffer for SPI commands to make it easier to send
    // everything in high-speed bursts
    vector<uint8_t> spidata;

public:
    LcdWriter();
    ~LcdWriter();

    /*
     * Queue up messages for the LCD
     * Methods ported from the microcontroller driver
     */
    void ComWrite(uint8_t data);
    void DataWrite(uint8_t data_high, uint8_t data_low);
    void ComDataWrite(uint8_t com, uint16_t data);

    /*
     * Writes all pending commands to the LCD
     * in one batch write operation
     */
    void SendAll();
};



#endif // LCD_WRITER_H_


