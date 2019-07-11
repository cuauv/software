//
//  udp_receiver.cpp
//  hydromathd
//
//  Code for receiving FPGA packets.
//
//  Created by Vlad on 9/10/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#include <cstdio>
#include <cstdint>
#include <sys/socket.h>
#include <netinet/in.h>

#include "udp_receiver.hpp"

static bool bound = 0;
static int receive_socket;
static struct sockaddr_in receive_serv_addr;

void udpReceiverInit()
{
    //Initializes the UDP networking for receiving FPGA packets.
    
    int r;
    
    receive_socket = socket(AF_INET, SOCK_DGRAM, 0);
    receive_serv_addr.sin_family = AF_INET;
    receive_serv_addr.sin_port = htons(udp_receive_port);
    receive_serv_addr.sin_addr.s_addr = INADDR_ANY;
    
    r = bind(receive_socket, (struct sockaddr *) &receive_serv_addr, sizeof(receive_serv_addr));
    
    if(r == 1)
    {
        printf("%s \n", "binding receive socket failed!");
    }
    else
    {
        bound = 1;
        printf("%s \n", "bound receive socket successfully");
    }
}

bool udpReceive(uint16_t *fpga_packet)
{
    //Uses a blocking socket to receive UDP packets.
    
    recvfrom(receive_socket, fpga_packet, udp_receive_payload_size, 0, NULL, NULL);
    
    return 0;
}
