//
//  constants.cpp
//  hydromathd
//
//  Created by Vlad on 7/3/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#include <netinet/in.h>

namespace Comms
{
    extern const unsigned int DSP_DECIM_FACTOR = 20;
    extern const unsigned int CARRIER_FREQ = 52'000;
    extern const unsigned int BANDWIDTH = 3'000;
    extern const int SYNCH_SYMBOLS[2] = {-1'000, 1'000};
    extern const int SYMBOLS[2] = {-1'000, 1'000};
    extern const float SYM_CORRECTIONS[2] = {3.0, 1.0};
    extern const float SYNCH_SYM_CORRECTIONS[2] = {3.0, 1.0};
    extern const int BITS_PER_SYM = 1;
    extern const int SYM_RATE = 8;
    extern const float CODE[32] = {1, 1, 1, 1, 1, -1, -1, 1, 1, -1, 1, -1, -1, 1, -1, -1, -1, -1, 1, -1, 1, -1, 1, 1, 1, -1, 1, 1, -1, -1, -1, 0};
    extern const float ORTH_CODE[32] = {1, 1, 1, 1, 1, -1, -1, 1, -1, -1, 1, 1, -1, -1, -1, -1, 1, -1, 1, 1, -1, 1, -1, 1, -1, -1, -1, 1, 1, 1, -1, 0};
    extern const unsigned int CODE_LEN = 32;
    extern const unsigned int PKT_SIZE = 4;
    
    extern const char BASEB_PLOT_ADDR[] = "127.0.0.1";
    extern const unsigned int BASEB_PLOT_PORT = 9004;
    extern const unsigned int BASEB_PLOT_PER = 200'000;
    extern const unsigned int BASEB_PLOT_LEN = 10'000;
    
    extern const char CORR_PLOT_ADDR[] = "127.0.0.1";
    extern const unsigned int CORR_PLOT_PORT = 9005;
    extern const unsigned int CORR_PLOT_LEN = 1'000;
}

extern const unsigned int BUFF_LEN = 65536;
extern const unsigned int DC_AVG_PER = 20'000;

extern const float BIT_DEPTH = 16383;
extern const unsigned int ADC_SAMPLE_RATE = 200'000;
extern const unsigned int MAX_GAIN_LVL = 13;

extern const unsigned int SAMPLE_PORT = 8899;
extern const unsigned int SAMPLE_PKT_LEN = 128;

extern const char GAIN_ADDR[] = "192.168.93.13";
extern const unsigned int GAIN_PORT = 5005;

extern const char RAW_PLOT_ADDR[] = "127.0.0.1";
extern const unsigned int RAW_PLOT_PORT = 9001;

extern const char TRIGGER_PLOT_ADDR[] = "127.0.0.1";
extern const unsigned int TRIGGER_PLOT_PORT = 9002;

extern const char DFT_PLOT_ADDR[] = "127.0.0.1";
extern const unsigned int DFT_PLOT_PORT = 9003;

extern const unsigned int GAIN_PKT_SIZE = 2;
extern const unsigned int PLOT_PKT_SIZE = 512;
