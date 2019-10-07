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
#include <arpa/inet.h>

#include "udp_receiver.hpp"

UDPSampleReceiver::UDPSampleReceiver(unsigned int port):
sock(socket(AF_INET, SOCK_DGRAM, 0))
{
    int status;
    struct sockaddr_in serv_addr;
    
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    
    status = bind(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    if(status < 0)
    {
        printf("%s \n", "binding receive socket failed!");
    }
    else
    {
        printf("%s \n", "bound receive socket successfully");
    }
}

void UDPSampleReceiver::recv(uint16_t *pkt, unsigned int pkt_len)
{
    recvfrom(sock, pkt, pkt_len * 4 * sizeof(uint16_t), 0, NULL, NULL);
}
