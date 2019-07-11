//
//  hydromathd.cpp
//  hydromathd
//
//  Top level source file. Receives the raw fpga packets and calls pinger tracking code (if enabled) on each of them.
//  Read the code documentation only after reading the Hydrophones Code wiki entry.
//
//  Created by Vlad on 9/10/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#include <cstdio>

#include "libshm/c/vars.h"
//#include "shm_mac.hpp"
#include "common_dsp.hpp"
#include "pinger_tracking.hpp"
//#include "comms.hpp"
#include "udp_receiver.hpp"
#include "udp_sender.hpp"

//extern FILE *audible_file;

int main()
{
    int current_mode = 0;
	struct hydrophones_settings shm_settings;
	struct hydrophones_status shm_status;

    //initializing modules for receiving/sending network data and handling shm
    udpReceiverInit();
    udpSenderInit();
    //commsInit();
    shm_init();
    
    //audible_file = fopen("audible_file.txt", "w");
    
    shm_status.packet_count = 0; 

    while(1) //program superloop
    {
        uint16_t fpga_packet[4 * packet_length];
        
        shm_getg(hydrophones_settings, shm_settings);
        
        //receiving a packet via UDP
        udpReceive(fpga_packet);
        
        //calling pinger tracking code if in tracking mode or communications receive code if in comms mode
        if(shm_settings.choose_one_for_tracking_choose_zero_for_communications == 1)
        {
            pingerTrackingDSP(fpga_packet, current_mode != 1);
            current_mode = 1;
        }
        else
        {
            if(current_mode != 0)
            {
                //commsReset();
                current_mode = 0;
            }
            
            //commsDSP(fpga_packet, shm_status.packet_count);
        }

        shm_status.packet_count++;
        shm_setg(hydrophones_status, shm_status);
        
        //fflush(audible_file);
    }
    
    //toodles!!!!!
    return 0;
}

// vim: tabstop=4 softtabstop=4 shiftwidth=4
