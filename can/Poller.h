#pragma once

#include "lib/CANSocket.h"
#include "lib/MessageBuilder.h"

#include "Thread.h"
#include "Registry.h"

class Poller : public Thread {
    public:
        Poller(std::shared_ptr<MessageBuilder> builder, std::shared_ptr<Registry> registry, std::shared_ptr<CANSocket> sock);

    private:
        virtual void run();

        void sendGroups() const;

        std::shared_ptr<MessageBuilder> m_builder;
        std::shared_ptr<Registry> m_registry;
        std::shared_ptr<CANSocket> m_sock;
};
