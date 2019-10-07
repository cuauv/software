//
//  udp_receiver.hpp
//  hydromathd
//
//  Created by Vlad on 9/10/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#ifndef udp_receiver_hpp
#define udp_receiver_hpp

#include <cstdint>
#include <netinet/in.h>

class UDPSampleReceiver
{
public:
    UDPSampleReceiver(unsigned int port);
    void recv(uint16_t *pkt, unsigned int pkt_len);

private:
    int sock;
};

#endif /* udp_receiver_hpp */
