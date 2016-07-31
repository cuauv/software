#include "Manager.h"

#include "lib/Log.h"

#include "ShmMessageBuilder.h"

Manager::Manager(const char* iface, std::shared_ptr<Registry> registry) :
            m_iface(iface),
            m_builder(std::make_shared<ShmMessageBuilder>(0)),
            m_registry(registry),
            m_handler(m_builder, m_registry, getSocket()),
            m_poller(m_builder, m_registry, getSocket()),
            m_culler(m_registry),
            m_senable(false)
{

}

Manager::~Manager() {
    shutdown();
}

void Manager::init() {
    Log::log("Initializing Manager");
    // Check for other hosts
    m_handler.launch();
    m_poller.launch();
    m_culler.launch();
}

void Manager::shutdown() {
    Log::log("Shutting down Culler");
    m_culler.shutdown();
    Log::log("Shutting down Handler");
    m_handler.shutdown();

    Log::log("Clearing devices");
    m_registry->clear();

    Log::log("Shutting down Poller");
    m_poller.shutdown();

    Log::log("Moving all devices to group 0");
    auto sock = getSocket();
    Message haltMsg(Message::Type::GROUP, 0, Selector::Broadcast(), false, 0, false, {});
    sock->sendMessage(haltMsg);
}

void Manager::setSetpoint(uint8_t devid, int16_t setpoint) {
    // Lookup device
    auto dev = m_registry->getDevice(devid);
    if (dev == nullptr) return;

    // Get group and offset for device
    auto group = m_registry->getGroup(dev->getGroup());
    if (group == nullptr) return;
    auto offset = dev->getOffset();

    // Write new setpoint
    group->setSetpoint(offset, setpoint);
}

std::shared_ptr<CANSocket> Manager::getSocket() const {
    return std::make_shared<CANSocket>(m_iface);
}

bool Manager::getSoftEnable() const {
    return m_builder->getEnable();
}

void Manager::setSoftEnable(bool senable) {
    m_builder->setEnable(senable);
}
