#pragma once
#include <thread>
#include <memory>
#include <map>
#include <mutex>

#include "Device.h"
#include "Group.h"
#include "Handler.h"
#include "Poller.h"
#include "Culler.h"

class Manager {
    public:
        Manager(const char* iface, std::shared_ptr<Registry> registry);
        ~Manager();

        void init();
        void shutdown();

        void setSetpoint(uint8_t devid, int16_t setpoint);

        void forEachDevice(std::function<void(std::shared_ptr<Device>)> callback);
        void forEachGroup(std::function<void(std::shared_ptr<Group>)> callback);

        std::shared_ptr<CANSocket> getSocket() const;

        bool getSoftEnable() const;
        void setSoftEnable(bool enable);

    private:
        std::mutex m_devLock;
        std::mutex m_grpLock;

        std::map<uint8_t, std::shared_ptr<Device>> m_devices;
        std::map<uint8_t, std::shared_ptr<Group>> m_groups;

        const char* const m_iface;
        
        std::shared_ptr<MessageBuilder> m_builder;
        std::shared_ptr<Registry> m_registry;

        Handler m_handler;
        Poller m_poller;
        Culler m_culler;

        bool m_senable;
};
