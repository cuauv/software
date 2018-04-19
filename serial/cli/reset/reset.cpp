#include <iostream>
#include <functional>
#include <condition_variable>

#include <serial/libserial/Manager.h>

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

using namespace std::placeholders;

class ResetDevice {
    private:
        Manager m_mgr;
        std::condition_variable m_cond;
        std::mutex m_lck;
        bool m_deviceFound;

        class DummyCallbacks : public DeviceCallbacks {
            private:
                ResetDevice* m_cmd;
            public:
                DummyCallbacks(ResetDevice* cmd) : m_cmd(cmd) {}
                void onRead(const std::unordered_set<BoundVariable>&) {}
                void onWrite(const std::unordered_set<BoundVariable>&) {}
                bool getSoftKill() {
                    m_cmd->m_cond.notify_all();
                    return true;
                }
                void postHardKill(bool, bool) {}
                void onDisconnect() {}
        };

        std::shared_ptr<DeviceCallbacks> onConnected(Manager*, std::shared_ptr<DeviceInfo> info) {
            std::lock_guard<std::mutex> lock(m_lck);
            std::cout << "Found device " << info->name() << std::endl;
            m_deviceFound = true;
            return std::make_shared<DummyCallbacks>(this);
        }

        void onPortFailed(Manager*, std::string msg) {
            std::cout << "Port error: " << msg << std::endl;
            m_cond.notify_all();
        }
    public:
        ResetDevice(const std::string& port) : 
            m_mgr(port,
                  std::bind(&ResetDevice::onConnected, this, _1, _2),
                  std::bind(&ResetDevice::onPortFailed, this, _1, _2))
        {
        }
        
        int run() {
            std::unique_lock<std::mutex> lock(m_lck);
            std::cout << "Searching for device" << std::endl;
            m_mgr.start();
            if (m_cond.wait_for(lock, std::chrono::seconds(2)) == std::cv_status::timeout) {
                std::cout << "No device found, attempting reset anyways" << std::endl;
                return 0;
            } else if (m_deviceFound) {
                std::cout << "Resetting..." << std::endl;
                m_mgr.resetDevice();
                return 0;
            } else {
                return 1;
            }
        }

};

static int reset_run(std::map<std::string, std::string> args) {
    try {
        ResetDevice cmd(args["port"]);
        cmd.run();
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(reset, "Reset a connected device", reset_run)
ARG_REQUIRED(port, "The device's port")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
