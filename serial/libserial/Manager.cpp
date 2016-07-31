#include "Manager.h"

#include "UnixFilePort.h"
#include "SerialPort.h"

#include "packet_types/All.h"

// Send heartbeats every 150ms
#define HEARTBEAT_INTERVAL 100
// Probe for a device every 1 second
#define HELLO_INTERVAL 1000

namespace cuauv {
namespace serial {

Manager::Manager(std::string port, connect_callback_t onConnected, failure_callback_t onPortFailed) :
            m_threadRunning(false),
            m_talker(getPort(port)),
            m_onConnect(onConnected),
            m_onPortFailure(onPortFailed),
            m_devCallbacks(nullptr)
{

}

Manager::Manager(std::unique_ptr<Port> port, connect_callback_t onConnected, failure_callback_t onPortFailed) :
            m_threadRunning(false),
            m_talker(std::move(port)),
            m_onConnect(onConnected),
            m_onPortFailure(onPortFailed),
            m_devCallbacks(nullptr)
{

}

Manager::~Manager() {
    stop();
    if (m_thread.joinable()) {
        m_thread.join();
    }
}

std::unique_ptr<Port> Manager::getPort(std::string portId) {
    auto split = portId.find(':');
    if (split != std::string::npos) {
        // This is a special port type
        std::string type = portId.substr(0, split);
        std::string path = portId.substr(split+1);
        if (type == "fifo") {
            return std::unique_ptr<Port>(new UnixFilePort(path + "_TX", path + "_RX", portId));
        } else {
            // unknown port type
            throw std::invalid_argument("Unknown port type " + type); 
        }
    } else {
        return std::unique_ptr<Port>(new SerialPort(portId));
    }
}

void Manager::start() {
    if (!isRunning()) {
        // clear all the queues
        m_readQueue = {};
        m_writeQueue = {};
        m_periodicReadQueue = {};

        m_shouldTerminate = false;
        m_thread = std::thread(&Manager::run, this);
        m_threadRunning = true;
    }
}

std::string Manager::portName() const {
    // portName is const in talker/port, don't need to lock
    return m_talker.portName();
}

bool Manager::isRunning() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_threadRunning;
}

bool Manager::isConnected() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    // We always have a callback object while connected,
    // and no callback object when not
    return m_devCallbacks != nullptr;
}

void Manager::trigger_shutdown() {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_shouldTerminate = true;
    m_cond.notify_all();
}

void Manager::stop() {
    if (isRunning()) {
        trigger_shutdown();
        m_thread.join();
    }
}

void Manager::disconnect() {
    if (isRunning()) {
        stop();
        try {
            m_talker.send(std::make_shared<DisconnectRequest>());
        } catch (std::exception& e) {
            // Ignore if the disconnect message fails
        }
    }
}

void Manager::resetDevice() {
    if (isRunning()) {
        stop();
        try {
            m_talker.send(std::make_shared<ResetRequest>());
        } catch (std::exception& e) {
            // Ignore if the reset message fails
        }
    }
}

void Manager::submitRead(const std::unordered_set<Variable>& vars) {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_readQueue.push_back(vars);
    m_cond.notify_all();
}

void Manager::submitWrite(const std::unordered_set<BoundVariable>& vars) {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_writeQueue.push_back(vars);
    m_cond.notify_all();
}

std::set<std::string> Manager::listPorts() {
    return SerialPort::listPorts();
}

void Manager::deviceDisconnected() {
    m_devCallbacks->onDisconnect();
    {
        // m_devCallbacks is technically used for communicating between threads,
        // so need to lock here
        std::lock_guard<std::mutex> lck(m_mutex);
        m_devCallbacks = nullptr;
    }
}

void Manager::processHeartbeat() {
    auto now = std::chrono::steady_clock::now();

    if (m_nextHeartbeat <= now) {
        auto request = std::make_shared<HeartbeatRequest>(m_devCallbacks->getSoftKill());
        auto response = std::static_pointer_cast<HeartbeatResponse>(m_talker.query(request));
        if (!response) {
            deviceDisconnected();
        } else {
            m_devCallbacks->postHardKill(response->isHardKillValid(), response->getHardKill());
            m_nextHeartbeat = now + std::chrono::milliseconds(HEARTBEAT_INTERVAL);
        }
    }
}

void Manager::processPendingReads() {
    std::unordered_set<Variable> readVars;

    // Drop in scope to get lock on read queue
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        for (auto read : m_readQueue) {
            readVars.insert(read.begin(), read.end());
        }
        m_readQueue.clear();
    }

    // check for any periodic reads that are ready to go
    auto now = std::chrono::steady_clock::now();
    auto it = m_periodicReadQueue.begin();
    std::vector<std::pair<int, std::unordered_set<Variable>>> periodicReads;
    while (it != m_periodicReadQueue.end() && it->first <= now) {
        periodicReads.push_back(it->second);
        it = m_periodicReadQueue.erase(it);
    }

    // scan the reads and add them back to the periodic schedule
    for (auto read : periodicReads) {
        readVars.insert(read.second.begin(), read.second.end());
        // calculate the time for the next read
        auto time = now + std::chrono::milliseconds(read.first);
        m_periodicReadQueue.insert(std::make_pair(time, read));
    }

    if (readVars.size() == 0) return;

    RegisterSet rset;
    for (auto var : readVars) {
        var.addToSet(rset);
    }

    if (rset.rangeIsSmaller()) {
        // do the read as a range read
        auto request = std::make_shared<ReadRangeRequest>(rset.toRange());
        auto response = std::static_pointer_cast<ReadRangeResponse>(m_talker.query(request));
        if (!response) {
            deviceDisconnected();
        } else {
            auto boundRange = response->getRange();
            std::unordered_set<BoundVariable> boundVars;
            for (auto var : readVars) {
                boundVars.insert(var.bind(boundRange));
            }
            m_devCallbacks->onRead(boundVars);
        }
    } else {
        // do the read as an indexed read
        auto request = std::make_shared<ReadIndexedRequest>(rset);
        auto response = std::static_pointer_cast<ReadIndexedResponse>(m_talker.query(request));
        if (!response) {
            deviceDisconnected();
        } else {
            auto boundSet = response->getRegs();
            std::unordered_set<BoundVariable> boundVars;
            for (auto var : readVars) {
                boundVars.insert(var.bind(boundSet));
            }
            m_devCallbacks->onRead(boundVars);
        }
    }
}

void Manager::processPendingWrites() {
    std::unordered_set<BoundVariable> writeVars;

    // Drop in scope to get lock on read queue
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        if (m_writeQueue.size() == 0) return;
        for (auto it = m_writeQueue.rbegin(); it != m_writeQueue.rend(); it++) {
            writeVars.insert(it->begin(), it->end());
        }
        m_writeQueue.clear();
    }

    BoundRegisterSet rset;
    for (auto var : writeVars) {
        var.bindInSet(rset);
    }

    std::shared_ptr<Request> request;
    //if (rset.rangeIsSmaller()) {
    //    request = std::make_shared<WriteRangeRequest>(rset.toBoundRange());
    //} else {
        request = std::make_shared<WriteIndexedRequest>(rset);
    //}
    auto response = m_talker.query(request); // response is only an ack, don't bother casting
    if (!response) {
        deviceDisconnected();
    } else {
        m_devCallbacks->onWrite(writeVars);
    }
}

void Manager::waitForNextEvent() const {
    // unique_lock here instead of lock_guard because m_cond only waits with unique_lock
    std::unique_lock<std::mutex> lck(m_mutex);

    // Bail out if there's stuff in the read or write queue, or if we need to exit
    if (!m_readQueue.empty() || !m_writeQueue.empty() || m_shouldTerminate) return;

    auto next_event = m_nextHeartbeat;
    if (!m_periodicReadQueue.empty()) {
        next_event = std::min(next_event, m_periodicReadQueue.begin()->first);
    }

    // We may get woken up earlier if someone signals the cond variable
    m_cond.wait_until(lck, next_event);
}

void Manager::pollForDevice() {
    auto request = std::make_shared<HelloRequest>();
    auto response = std::static_pointer_cast<HelloResponse>(m_talker.query(request));
    if (response) {
        // Clear all queues
        m_readQueue.clear();
        m_writeQueue.clear();
        m_periodicReadQueue.clear();

        auto deviceInfo = response->getDeviceInfo();
        auto callbacks = m_onConnect(this, deviceInfo);

        // If we get nullptr back, that means the client rejected the device for some reason
        // Don't proceed with initialization
        if (!callbacks) {
            std::this_thread::sleep_for(std::chrono::milliseconds(HELLO_INTERVAL));
            return;
        }

        // Register the poll groups
        auto now = std::chrono::steady_clock::now();
        for (auto group : deviceInfo->pollGroups()) {
            if (group.first == 0) continue; // ignore groups with an interval of 0

            // have all groups initially pending for read
            m_periodicReadQueue.insert(std::make_pair(now, group));
        }

        // Reset the heartbeat timer, will trigger a heartbeat as the next message sent
        m_nextHeartbeat = now;

        // Need to lock here while modifying the callbacks pointer, since it indicates that we've connected
        {
            std::lock_guard<std::mutex> lck(m_mutex);
            m_devCallbacks = callbacks;
        }
    } else {
        std::this_thread::sleep_for(std::chrono::milliseconds(HELLO_INTERVAL));
    }
}

// The main communication loop goes here
void Manager::run() {
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        m_threadRunning = true;
        m_devCallbacks = nullptr;
    }

    try {
        m_talker.waitDeviceIdle();

        while(!m_shouldTerminate) {
            if (m_devCallbacks) {
                // We have a connection to the device
                // Heartbeat needs to come first so that it's the first packet following the Hello exchange
                // Anything else before this will fail, because devices ignore packets other than
                // heartbeat directly following a Hello exchange
                processHeartbeat();
                // each processing step might have discovered the device disconnecting
                // so we need to check for that between each
                if (!m_devCallbacks) continue;
                processPendingReads();
                if (!m_devCallbacks) continue;
                processPendingWrites();
                if (!m_devCallbacks) continue;
                waitForNextEvent(); // sleeps until the next event
            } else {
                pollForDevice();
            }
        }
    } catch (std::exception& e) {
        // TODO: Should we catch port errors and handle them differently?
        if (m_devCallbacks) {
            deviceDisconnected();
        }
        m_onPortFailure(this, e.what());
    }

    if (m_devCallbacks) {
        deviceDisconnected();
    }

    {
        std::lock_guard<std::mutex> lck(m_mutex);
        m_threadRunning = false;
    }
}

}} // end namespace cuauv::serial
