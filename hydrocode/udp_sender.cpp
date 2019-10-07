//
//  udp_sender.cpp
//  hydromathd
//
//  Code for sending plots to Python plotting scripts and gain settings to the FPGA, via UDP.
//
//  Created by Vlad on 9/15/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#include <cstdio>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "udp_sender.hpp"

const unsigned int UDPGainSender::GAIN_PKT_SIZE = 2;
const unsigned int UDPPlotSender::PLOT_PKT_SIZE = 512;

UDPGainSender::UDPGainSender(const char addr[], unsigned int port):
sock(socket(AF_INET, SOCK_DGRAM, 0))
{
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = inet_addr(addr);
    
    if(sock == -1)
    {
        printf("%s \n", "failed to create gain socket!");
    }
    else
    {
        printf("%s \n", "created gain socket successfully");
    }
}

void UDPGainSender::send(unsigned int gain_lvl)
{
    char pkt[2];
    
    pkt[0] = (gain_lvl + 1) / 10 + '0';
    pkt[1] = (gain_lvl + 1) % 10 + '0';
    
    sendto(sock, pkt, GAIN_PKT_SIZE, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
}


UDPPlotSender::UDPPlotSender(const char addr[], unsigned int port):
sock(socket(AF_INET, SOCK_DGRAM, 0))
{
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = inet_addr(addr);

    if(sock == -1)
    {
        printf("%s \n", "failed to create plot socket!");
    }
    else
    {
         printf("%s \n", "created plot socket successfully");
    }
}

void UDPPlotSender::send(float *values, unsigned int len)
{
    const unsigned int no_full_packets = len * sizeof(float) / PLOT_PKT_SIZE;
    const unsigned int partial_pkt_size =  len * sizeof(float) % PLOT_PKT_SIZE;
    
    for(unsigned int pkt_no = 0; pkt_no < no_full_packets; pkt_no++)
    {
        sendto(sock, values + pkt_no * PLOT_PKT_SIZE / sizeof(float), PLOT_PKT_SIZE, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    }
    
    if(partial_pkt_size != 0)
    {
        sendto(sock, values + no_full_packets * PLOT_PKT_SIZE / sizeof(float), partial_pkt_size, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    }
}
