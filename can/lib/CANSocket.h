#pragma once

#include <memory>
#include <sys/types.h>

#include "Message.h"

class CANSocket {
    public:
        CANSocket(const char* ifname);
        void setWriteOnly();
        void sendMessage(Message msg) const;
        std::shared_ptr<Message> readMessage(struct timeval* timeout) const;

        // Disallow copy and assign
        CANSocket(const CANSocket&) = delete;
        CANSocket& operator=(const CANSocket&) = delete;

    private:
        int m_socket;
};
