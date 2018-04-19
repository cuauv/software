#include "FakeDevice.h"

#include <iostream>

#include <UnixFilePort.h>
#include <SerialPort.h>

// incredibly arbitrary timeout value
#define REQUEST_TIMEOUT 25
#define T_HBT 250

namespace cuauv {
namespace serial {
namespace cli {

FakeDevice::FakeDevice(std::string portId, std::shared_ptr<DeviceInfo> info) :
        m_shouldTerminate(false),
        m_threadRunning(false),
        m_readRegs(0, 0), // will properly initialize in a moment
        m_softKill(true),
        m_hardKill(true),
        m_hardKillValid(false),
        m_isConnected(false),
        m_gotHello(false),
        m_devInfo(info),
        m_talker(getPort(portId))
{
    // Figure out what the range of readable registers is for this device
    RegisterSet regSet;
    for (auto var : m_devInfo->readVariables()) {
        var.addToSet(regSet);
    }
    regSet.insert(0); // The valid register range always starts at 0
    auto unboundRange = regSet.toRange();
    // Initialize the read register range. It'll start with garbage values, we expect
    // the user of this object to perform any initialization before calling start()
    m_readRegs = BoundRegisterRange(unboundRange.startRegister(), unboundRange.numRegisters());

    // Initialize the set of writeable variables
    // We'll bind 0s to them initially, until the host
    // sends actual data
    for (auto var : m_devInfo->writeVariables()) {
        if (var.type() == Variable::Type::FLOAT) {
            m_writeVars.insert(std::make_pair(var.name(), var.bind(0.0f)));
        } else {
            m_writeVars.insert(std::make_pair(var.name(), var.bind(0)));
        }
    }
}

std::unique_ptr<Port> FakeDevice::getPort(std::string portId) {
    auto split = portId.find(':');
    if (split != std::string::npos) {
        // This is a special port type
        std::string type = portId.substr(0, split);
        std::string path = portId.substr(split+1);
        if (type == "fifo") {
            // flip RX and TX from what Manager chooses
            return std::unique_ptr<Port>(new UnixFilePort(path + "_RX", path + "_TX", portId));
        } else {
            // unknown port type
            throw std::invalid_argument("Unknown port type " + type); 
        }
    } else {
        return std::unique_ptr<SerialPort>(new SerialPort(portId));
    }
}

std::string FakeDevice::portName() const {
    return m_talker.portName();
}

void FakeDevice::start() {
    if (!isRunning()) {
        m_shouldTerminate = false;
        m_thread = std::thread(&FakeDevice::run, this);
    }
}

void FakeDevice::trigger_shutdown() {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_shouldTerminate = true;
}

void FakeDevice::stop() {
    if (isRunning()) {
        trigger_shutdown();
        m_thread.join();
    }
}

bool FakeDevice::isRunning() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_threadRunning;
}

bool FakeDevice::isConnected() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_isConnected;
}

void FakeDevice::setReadVar(const BoundVariable& var) {
    std::lock_guard<std::mutex> lck(m_mutex);
    var.bindInRange(m_readRegs);
}

std::map<std::string, BoundVariable> FakeDevice::getWriteVars() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_writeVars;
}

bool FakeDevice::getSoftKill() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_softKill || !m_isConnected;
}

void FakeDevice::setHardKill(bool hardKill) {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_hardKill = hardKill;
}

bool FakeDevice::getHardKill() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_hardKill;
}

void FakeDevice::setHardKillValid(bool hardKillValid) {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_hardKillValid = hardKillValid;
}

bool FakeDevice::getHardKillValid() const {
    std::lock_guard<std::mutex> lck(m_mutex);
    return m_hardKillValid;
}

void FakeDevice::doDisconnect() {
    std::lock_guard<std::mutex> lck(m_mutex);
    m_isConnected = false;
}

void FakeDevice::handleHello(std::shared_ptr<HelloRequest> UNUSED(request)) {
    m_gotHello = true;
    m_talker.send(std::make_shared<HelloResponse>(m_devInfo));
}

void FakeDevice::handleReset(std::shared_ptr<ResetRequest> UNUSED(request)) {
    doDisconnect();
    m_gotHello = false;

    // No response
}

void FakeDevice::handleHeartbeat(std::shared_ptr<HeartbeatRequest> request) {
    bool hardKill;
    bool hardKillValid;

    auto heartbeatDuration = std::chrono::steady_clock::now() - m_lastHeartbeat;
    uint16_t heartbeatTime = std::chrono::duration_cast<std::chrono::milliseconds>(heartbeatDuration).count();

    {
        std::lock_guard<std::mutex> lck(m_mutex);
        m_softKill = request->getSoftKill();
        hardKill = m_hardKill;
        hardKillValid = m_hardKillValid;
        m_isConnected = true;
    }

    m_talker.send(std::make_shared<HeartbeatResponse>(hardKill, hardKillValid, heartbeatTime));

    m_lastHeartbeat = std::chrono::steady_clock::now();
}

void FakeDevice::handleReadRange(std::shared_ptr<ReadRangeRequest> request) {
    auto range = request->getRange();
    // TODO: Check range bounds

    BoundRegisterRange response_range(range.startRegister(), range.numRegisters());
    for (int i = 0; i < range.numRegisters(); i++) {
        auto reg = range.startRegister() + i;
        response_range.at(reg) = m_readRegs.at(reg);
    }

    m_talker.send(std::make_shared<ReadRangeResponse>(response_range));
}

void FakeDevice::handleWriteRange(std::shared_ptr<WriteRangeRequest> request) {
    auto range = request->getRange();
    // TODO: Check range bounds

    {
        std::lock_guard<std::mutex> lck(m_mutex);
        for (auto& var : m_writeVars) {
            if (var.second.isPartOf(range)) {
                var.second = var.second.bind(range);
            }
        }
    }

    m_talker.send(std::make_shared<WriteRangeResponse>());
}

void FakeDevice::handleReadIndexed(std::shared_ptr<ReadIndexedRequest> request) {
    auto set = request->getRegs();
    // TODO: Check set bounds

    BoundRegisterSet response_set;
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        for (auto reg : set) {
            response_set[reg] = m_readRegs.at(reg);
        }
    }

    m_talker.send(std::make_shared<ReadIndexedResponse>(response_set, request->getRegOrder()));
}

void FakeDevice::handleWriteIndexed(std::shared_ptr<WriteIndexedRequest> request) {
    auto set = request->getRegs();
    // TODO: Check set bounds
    
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        for (auto& var : m_writeVars) {
            if (var.second.isPartOf(set)) {
                var.second = var.second.bind(set);
            }
        }
    }

    m_talker.send(std::make_shared<WriteIndexedResponse>());
}

void FakeDevice::handleDisconnect(std::shared_ptr<DisconnectRequest> UNUSED(request)) {
    doDisconnect();

    // no response
}

void FakeDevice::run() {
    {
        std::lock_guard<std::mutex> lck(m_mutex);
        m_threadRunning = true;
        m_isConnected = false;
        m_gotHello = false;
    }

    try {
        while (!m_shouldTerminate) {
            auto request = m_talker.listen(REQUEST_TIMEOUT);
            if (request) {
                if (m_isConnected) {
                    switch (request->type()) {
                        case Packet::Type::Hello:
                            handleHello(std::static_pointer_cast<HelloRequest>(request));
                            break;
                        case Packet::Type::Reset:
                            handleReset(std::static_pointer_cast<ResetRequest>(request));
                            break;
                        case Packet::Type::Heartbeat:
                            handleHeartbeat(std::static_pointer_cast<HeartbeatRequest>(request));
                            break;
                        case Packet::Type::ReadRange:
                            handleReadRange(std::static_pointer_cast<ReadRangeRequest>(request));
                            break;
                        case Packet::Type::WriteRange:
                            handleWriteRange(std::static_pointer_cast<WriteRangeRequest>(request));
                            break;
                        case Packet::Type::ReadIndexed:
                            handleReadIndexed(std::static_pointer_cast<ReadIndexedRequest>(request));
                            break;
                        case Packet::Type::WriteIndexed:
                            handleWriteIndexed(std::static_pointer_cast<WriteIndexedRequest>(request));
                            break;
                        case Packet::Type::Disconnect:
                            handleDisconnect(std::static_pointer_cast<DisconnectRequest>(request));
                            break;
                        default:
                            m_talker.send(std::make_shared<ErrorResponse>(ErrorResponse::Error::TYPE));
                    }
                } else {
                    switch (request->type()) {
                        case Packet::Type::Hello:
                            handleHello(std::static_pointer_cast<HelloRequest>(request));
                            break;
                        case Packet::Type::Heartbeat:
                            if (m_gotHello) {
                                // only accept the heartbeat if the host has sent an initial Hello
                                handleHeartbeat(std::static_pointer_cast<HeartbeatRequest>(request));
                            }
                            break;
                        default:
                            // Don't do anything, we're disconnected
                            break;
                    }
                }
            }
            
            // Update the connected state
            if (std::chrono::steady_clock::now() - m_lastHeartbeat > std::chrono::milliseconds(T_HBT)) {
                doDisconnect();
            }
        }
    } catch (std::exception& e) {
        // TODO: Report errors
        std::cout << e.what() << std::endl;
    }

    {
        std::lock_guard<std::mutex> lck(m_mutex);
        m_threadRunning = false;
        m_isConnected = false;
    }
}

}}} // end namespace cuauv::serial::cli
