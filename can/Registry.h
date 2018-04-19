#pragma once

#include <memory>
#include <map>

#include "Device.h"
#include "Group.h"

class Registry {
    public:
        std::shared_ptr<Device> getDevice(uint8_t devid);
        virtual std::shared_ptr<Device> createDevice(uint8_t devid) = 0;

        std::shared_ptr<Group> getGroup(uint8_t gid);

        void forEachDevice(std::function<void(std::shared_ptr<Device>)> callback);
        void forEachGroup(std::function<void(std::shared_ptr<Group>)> callback);
        void cull();

        void clear();

    protected:
        void addDevice(std::shared_ptr<Device> dev);
        void addGroup(std::shared_ptr<Group> grp);

    private:
        std::mutex m_devLock;
        std::mutex m_grpLock;

        std::map<uint8_t, std::shared_ptr<Device>> m_devices;
        std::map<uint8_t, std::shared_ptr<Group>> m_groups;
};
