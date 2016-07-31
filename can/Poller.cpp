#include "Poller.h"
#include <libshm/c/shm.h>

#define ANNOUNCE_THRESH 20

Poller::Poller(std::shared_ptr<MessageBuilder> builder, std::shared_ptr<Registry> registry, std::shared_ptr<CANSocket> sock) : 
                    m_builder(builder),
                    m_registry(registry),
                    m_sock(sock)
{
    m_sock->setWriteOnly();
}

void Poller::run() {
    int announceTimer = 0;
    while (!shouldTerminate()) {
        auto loop_start = std::chrono::steady_clock::now();

        sendGroups();

        if (announceTimer++ > ANNOUNCE_THRESH) {
            m_sock->sendMessage(m_builder->buildMsg(Message::Type::ANNOUNCE, Selector::Group(0), 0, true, {}));
            announceTimer = 0;
        }

        std::this_thread::sleep_until(loop_start + std::chrono::milliseconds(100));
    }
}

void Poller::sendGroups() const {
    m_registry->forEachGroup([this] (std::shared_ptr<Group> grp) {
        auto data = grp->buildMessageData();
        m_sock->sendMessage(m_builder->buildMsg(Message::Type::VALUE, Selector::Group(grp->getGid()), 0, false, data));
    });

    // Send group 1 (always values of 0)
    m_sock->sendMessage(m_builder->buildMsg(Message::Type::VALUE, Selector::Group(1), 0, false, {0,0,0,0,0,0,0,0}));
    
    // Stepper as thruster hax
    MessageData stepdata;
    int16_t swaft_speed;
    shm_get(motor_desires, sway_aft, swaft_speed);
    stepdata.int16Push(swaft_speed);
    m_sock->sendMessage(m_builder->buildMsg(Message::Type::PARAM, Selector::DevID(20), 0, false, stepdata));
}
