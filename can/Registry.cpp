#include "Registry.h"

#include "lib/Log.h"
#include <set>

std::shared_ptr<Device> Registry::getDevice(uint8_t devid) {
    std::lock_guard<std::mutex> lck(m_devLock);
    auto it = m_devices.find(devid);
    if (it == m_devices.end()) return nullptr;
    else return it->second;
}

std::shared_ptr<Group> Registry::getGroup(uint8_t gid) {
    std::lock_guard<std::mutex> lck(m_grpLock);
    auto it = m_groups.find(gid);
    if (it == m_groups.end()) return nullptr;
    else return it->second;
}

void Registry::forEachDevice(std::function<void(std::shared_ptr<Device>)> callback) {
    std::lock_guard<std::mutex> lck(m_devLock);
    for (auto dev : m_devices) {
        callback(dev.second);
    }
}

void Registry::forEachGroup(std::function<void(std::shared_ptr<Group>)> callback) {
    std::lock_guard<std::mutex> lck(m_grpLock);
    for (auto grp : m_groups) {
        callback(grp.second);
    }
}

void Registry::cull() {
    std::lock_guard<std::mutex> lckDev(m_devLock);
    std::lock_guard<std::mutex> lckGrp(m_grpLock);

    std::set<uint8_t> seenGroups;
    // Can't use foreach loop b/c removing elements
    for (auto it = m_devices.begin(); it != m_devices.end();) {
        auto dev = (*it).second;

        // Check for dead devices
        if (dev->isDead()) {
            Log::log("Dropping device " + std::to_string((*it).first));
            dev->onDeviceCulled();
            it = m_devices.erase(it);
        } else {
            seenGroups.insert(dev->getGroup());
            it++;
        }
    }

    for (auto it = m_groups.begin(); it != m_groups.end();) {
        // If group not seen
        if (!seenGroups.count((*it).first)) {
            Log::log("Dropping group " + std::to_string((*it).first));
            it = m_groups.erase(it);
        } else {
            it++;
        }
    }
}

void Registry::clear() {
    std::lock_guard<std::mutex> lckDev(m_devLock);
    std::lock_guard<std::mutex> lckGrp(m_grpLock);

    for (auto dev : m_devices) {
        // Not sure why destructors don't get called automatically,
        // but need to do this to flag the device as dead in shm
        dev.second->onDeviceCulled();
    }

    m_devices.clear();
    m_groups.clear();
}

void Registry::addDevice(std::shared_ptr<Device> dev) {
    std::lock_guard<std::mutex> lck(m_devLock);
    m_devices[dev->getDevID()] = dev;
}

void Registry::addGroup(std::shared_ptr<Group> grp) {
    std::lock_guard<std::mutex> lck(m_grpLock);
    m_groups[grp->getGid()] = grp;
}
