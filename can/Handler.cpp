#include "Handler.h"

#include "lib/Log.h"

Handler::Handler(std::shared_ptr<MessageBuilder> builder, std::shared_ptr<Registry> registry, std::shared_ptr<CANSocket> sock) :
                                 m_builder(builder),
                                 m_registry(registry),
                                 m_sock(sock)
{
}

void Handler::run() {
    std::vector<Selector> selectors = {Selector::Broadcast(), Selector::DevID(0)};

    while (!shouldTerminate()) {
        try {
            struct timeval timeout;
            timeout.tv_sec = 2;
            timeout.tv_usec = 0;
            auto msg = m_sock->readMessage(&timeout);
            if (msg) {
                if (msg->matchesAny(selectors)) {
                    processMessage(*msg);
                }
            }
        } catch (std::exception& e) {
            Log::log("[HAND WARN] " + std::string(e.what()));
        }
    }
}

void Handler::processMessage(const Message& msg) {
    std::shared_ptr<Device> dev;
    if (msg.getType() == Message::Type::ANNOUNCE) {
        dev = m_registry->createDevice(msg.getSource());
    } else {
        dev = m_registry->getDevice(msg.getSource());
    }

    if (dev) {
        dev->processMessage(msg, *m_sock, m_builder);
    }
}
