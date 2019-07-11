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

static int gain_socket;
static struct sockaddr_in gain_serv_addr;
static int plot_sockets[5];
static struct sockaddr_in plot_serv_addresses[5];

void udpSenderInit()
{
    //Initializes the UDP networking for transmission.
    
    //raw plot
    plot_sockets[0] = socket(AF_INET, SOCK_DGRAM, 0);
    plot_serv_addresses[0].sin_family = AF_INET;
    plot_serv_addresses[0].sin_port = htons(udp_raw_plot_port);
    plot_serv_addresses[0].sin_addr.s_addr = inet_addr(UDP_RAW_PLOT_ADDRESS);

    if(plot_sockets[0] == -1)
    {
        printf("%s \n", "failed to create raw plot socket!");
    }
    else
    {
         printf("%s \n", "created raw plot socket successfully");
    }
    
    //trigger plot
    plot_sockets[1] = socket(AF_INET, SOCK_DGRAM, 0);
    plot_serv_addresses[1].sin_family = AF_INET;
    plot_serv_addresses[1].sin_port = htons(udp_trigger_plot_port);
    plot_serv_addresses[1].sin_addr.s_addr = inet_addr(UDP_TRIGGER_PLOT_ADDRESS);
    
    if(plot_sockets[1] == -1)
    {
        printf("%s \n", "failed to create trigger plot socket!");
    }
    else
    {
        printf("%s \n", "created trigger plot socket successfully");
    }
    
    //dft plot
    plot_sockets[2] = socket(AF_INET, SOCK_DGRAM, 0);
    plot_serv_addresses[2].sin_family = AF_INET;
    plot_serv_addresses[2].sin_port = htons(udp_dft_plot_port);
    plot_serv_addresses[2].sin_addr.s_addr = inet_addr(UDP_DFT_PLOT_ADDRESS);
    
    if(plot_sockets[2] == -1)
    {
        printf("%s \n", "failed to create dft plot socket!");
    }
    else
    {
        printf("%s \n", "created dft plot socket successfully");
    }
    
    plot_sockets[3] = socket(AF_INET, SOCK_DGRAM, 0);
    plot_serv_addresses[3].sin_family = AF_INET;
    plot_serv_addresses[3].sin_port = htons(udp_comms_filtered_plot_port);
    plot_serv_addresses[3].sin_addr.s_addr = inet_addr(UDP_COMMS_FILTERED_PLOT_ADDRESS);
    
    if(plot_sockets[3] == -1)
    {
        printf("%s \n", "failed to create comms filtered plot socket!");
    }
    else
    {
        printf("%s \n", "created comms filtered plot socket successfully");
    }
    
    plot_sockets[4] = socket(AF_INET, SOCK_DGRAM, 0);
    plot_serv_addresses[4].sin_family = AF_INET;
    plot_serv_addresses[4].sin_port = htons(udp_corr_plot_port);
    plot_serv_addresses[4].sin_addr.s_addr = inet_addr(UDP_CORR_PLOT_ADDRESS);
    
    if(plot_sockets[4] == -1)
    {
        printf("%s \n", "failed to create correlation plot socket!");
    }
    else
    {
        printf("%s \n", "created correlation plot socket successfully");
    }
    
    //gain settings
    gain_socket = socket(AF_INET, SOCK_DGRAM, 0);
    gain_serv_addr.sin_family = AF_INET;
    gain_serv_addr.sin_port = htons(udp_gain_port);
    gain_serv_addr.sin_addr.s_addr = inet_addr(UDP_GAIN_ADDRESS);
    
    if(gain_socket == -1)
    {
        printf("%s \n", "failed to create gain settings socket!");
    }
    else
    {
        printf("%s \n", "created gain settings socket successfully");
    }
}

void sendPlot(float *values, int plot_type, int plot_length)
{
    //Sends UDP packets to plotting scripts.
    
    int full_payload_size;
    int no_full_packets;
    int partial_payload_size;
    struct sockaddr_in serv_addr;
    int socket;

    full_payload_size = udp_send_payload_size * sizeof(float);
    no_full_packets = plot_length / udp_send_payload_size;
    partial_payload_size = plot_length % udp_send_payload_size * sizeof(float);
    serv_addr = plot_serv_addresses[plot_type];
    socket = plot_sockets[plot_type];
    
    for(int send_packet_no = 0; send_packet_no < no_full_packets; send_packet_no++)
    {
        sendto(socket, values + send_packet_no * udp_send_payload_size, full_payload_size, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
        //on every iteration, the data pointer is incremented by the payload length ("values + send_packet_no * udp_send_payload_length")
    }
    
    //data may not occupy an integer number of packets. one additional smaller packet may need to be sent
    if(partial_payload_size != 0)
    {
        sendto(socket, values + no_full_packets * udp_send_payload_size, partial_payload_size, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
        //the final piece of data is found at values + no_full_packets * udp_send_payload_length
    }
}

void sendGain(char *gain_packet)
{
    //Sends gain setting UDP packet to the FPGA.
    
    sendto(gain_socket, gain_packet, udp_gain_payload_size * sizeof(char), 0, (struct sockaddr *)&gain_serv_addr, sizeof(gain_serv_addr));
}
