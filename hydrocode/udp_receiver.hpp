//
//  udp_receiver.hpp
//  hydromathd
//
//  Created by Vlad on 9/10/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#ifndef udp_receiver_hpp
#define udp_receiver_hpp

static const int udp_receive_port = 8899; //FPGA sends packets to this port
static const int udp_receive_payload_size = 1024; //size of FPGA packet (in bytes). 1024 bytes/packet = 4 channels * 128 samples/packet/channel * 2 bytes/sample

void udpReceiverInit();
bool udpReceive(uint16_t *fpga_packet);

#endif /* udp_receiver_hpp */
