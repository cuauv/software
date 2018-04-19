#include "CANSocket.h"

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdexcept>
#include <assert.h>
#include <iostream>

#include <net/if.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <linux/can.h>
#include <linux/can/raw.h>

using namespace std;

class can_error : public std::exception {
    private:
    std::string m_msg;

    public:
    can_error(const std::string& msg) {
        char err[100];
        m_msg = msg + ": " + std::string(strerror_r(errno, err, sizeof(err)));
    }

    const char* what() const noexcept {
        return m_msg.c_str();
    }
};

CANSocket::CANSocket(const char* ifname) {
   struct sockaddr_can addr;
   struct ifreq ifr;

   if ((m_socket = socket(PF_CAN, SOCK_RAW, CAN_RAW)) < 0) {
       throw can_error("Failed to create socket");
   }

   strcpy(ifr.ifr_name, ifname);
   if (ioctl(m_socket, SIOCGIFINDEX, &ifr) < 0) {
       throw can_error("Failed to identify interface");
   }

   addr.can_family = AF_CAN;
   addr.can_ifindex = ifr.ifr_ifindex;

   if (bind(m_socket, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
       throw can_error("Failed to bind socket");
   }
}

void CANSocket::setWriteOnly() {
    setsockopt(m_socket, SOL_CAN_RAW, CAN_RAW_FILTER, NULL, 0);
}

void CANSocket::sendMessage(Message msg) const {
    struct can_frame frame;
    msg.fillFrame(frame);
    if(write(m_socket, &frame, sizeof(struct can_frame)) != sizeof(struct can_frame)) {
        throw can_error("Failure writing to socket");
    }
}

std::shared_ptr<Message> CANSocket::readMessage(struct timeval* timeout) const {
    fd_set if_set;
    FD_ZERO(&if_set);
    FD_SET(m_socket, &if_set);

    if (select(FD_SETSIZE, &if_set, NULL, NULL, timeout) < 0) {
        throw can_error("Failure while selecting socket");
    }

    if (FD_ISSET(m_socket, &if_set)) {
        struct can_frame frame;
        if (read(m_socket, &frame, sizeof(struct can_frame)) < 0) {
            throw can_error("Failure while reading from socket");
        }
        // Check EFF, ERR flags
        if (frame.can_id & CAN_ERR_FLAG || !(frame.can_id & CAN_EFF_FLAG)) {
            throw std::runtime_error("Bad flags in CAN frame");
        }

        return std::make_shared<Message>(frame);
    } else {
        return nullptr;
    }
}
