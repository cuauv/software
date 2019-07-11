//
//  common_dsp.cpp
//  hydromathd
//
//  Created by Vlad on 6/17/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#include "common_dsp.hpp"
#include "udp_sender.hpp"

void setGain(int gain_lvl)
{
    //Prepares and sends gain settings to the FPGA.
    
    char gain_packet[2]; //changing the gain requires sending the setting in a string format to the FPGA
    
    gain_packet[0] = (gain_lvl + 1) / 10 + '0';
    gain_packet[1] = (gain_lvl + 1) % 10 + '0';
    
    sendGain(gain_packet);
}
