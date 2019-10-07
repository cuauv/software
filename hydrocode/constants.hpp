//
//  constants.hpp
//  hydromathd
//
//  Created by Vlad on 7/3/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#ifndef constants_hpp
#define constants_hpp

namespace Comms
{
    extern const unsigned int DSP_DECIM_FACTOR;
    extern const unsigned int CARRIER_FREQ;
    extern const unsigned int BANDWIDTH;
    extern const int SYNCH_SYMBOLS[2];
    extern const int SYMBOLS[2];
    extern const float SYM_CORRECTIONS[2];
    extern const float SYNCH_SYM_CORRECTIONS[2];
    extern const int BITS_PER_SYM;
    extern const int SYM_RATE;
    extern const float CODE[32];
    extern const float ORTH_CODE[32];
    extern const unsigned int CODE_LEN;
    extern const unsigned int PKT_SIZE;
    
    extern const char BASEB_PLOT_ADDR[];
    extern const unsigned int BASEB_PLOT_PORT;
    extern const unsigned int BASEB_PLOT_PER;
    extern const unsigned int BASEB_PLOT_LEN;
    
    extern const char CORR_PLOT_ADDR[];
    extern const unsigned int CORR_PLOT_PORT;
    extern const unsigned int CORR_PLOT_LEN;
}

extern const unsigned int BUFF_LEN;
extern const unsigned int DC_AVG_PER;

extern const float BIT_DEPTH;
extern const unsigned int ADC_SAMPLE_RATE;
extern const unsigned int MAX_GAIN_LVL;

extern const unsigned int SAMPLE_PORT;
extern const unsigned int SAMPLE_PKT_LEN;

extern const char GAIN_ADDR[];
extern const unsigned int GAIN_PORT;

extern const char RAW_PLOT_ADDR[];
extern const unsigned int RAW_PLOT_PORT;

extern const char TRIGGER_PLOT_ADDR[];
extern const unsigned int TRIGGER_PLOT_PORT;

extern const char DFT_PLOT_ADDR[];
extern const unsigned int DFT_PLOT_PORT;

extern const unsigned int GAIN_PKT_SIZE;
extern const unsigned int PLOT_PKT_SIZE;

#endif /* constants_hpp */
