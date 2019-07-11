//
//  common_dsp.hpp
//  hydromathd
//
//  Created by Vlad on 11/8/18.
//  Copyright © 2018 Vlad Mihai. All rights reserved.
//

#ifndef common_dsp_hpp
#define common_dsp_hpp

static const float highest_quantization_lvl = 16383; //maximum possible level of a signal
static const unsigned int packet_length = 128; //number of samples for a channel in an FPGA packet
static const unsigned int sampling_rate = 200000; //self explanatory, no?

void setGain(int gain_lvl);

#endif /* common_dsp_hpp */
