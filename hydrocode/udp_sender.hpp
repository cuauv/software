//
//  udp_sender.hpp
//  hydromathd
//
//  Created by Vlad on 9/15/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#ifndef udp_sender_hpp
#define udp_sender_hpp

#include <netinet/in.h>

class UDPGainSender
{
public:
    UDPGainSender(const char addr[], unsigned int port);
    void send(unsigned int gain_lvl);
private:
    static const unsigned int GAIN_PKT_SIZE;
    
    int sock;
    struct sockaddr_in serv_addr;
};

class UDPPlotSender
{
public:
    UDPPlotSender(const char addr[], unsigned int port);
    void send(float *values, unsigned int len);
private:
    static const unsigned int PLOT_PKT_SIZE;
    
    int sock;
    struct sockaddr_in serv_addr;
};

#endif /* udp_sender_hpp */
