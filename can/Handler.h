#pragma once

#include "lib/CANSocket.h"
#include "lib/MessageBuilder.h"

#include "Thread.h"
#include "Registry.h"

class Handler : public Thread {
    public:
        Handler(std::shared_ptr<MessageBuilder> builder, std::shared_ptr<Registry> registry, std::shared_ptr<CANSocket> sock);

    private:
        virtual void run();

        void processMessage(const Message& msg);

        std::shared_ptr<MessageBuilder> m_builder;
        std::shared_ptr<Registry> m_registry;
        std::shared_ptr<CANSocket> m_sock;
};
