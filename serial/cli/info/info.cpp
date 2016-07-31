#include <iostream>
#include <functional>
#include <condition_variable>

#include <serial/libserial/Manager.h>

#include "../Command.h"

namespace cuauv {
namespace serial {
namespace cli {

using namespace std::placeholders;

static void showVars(const std::unordered_set<Variable>& vars, const std::map<std::string, BoundVariable> &defaults) {
    for (auto var : vars) {
        std::string typestr;
        switch(var.type()) {
            case Variable::Type::UINT8:
                typestr = "uint8 ";
                break;
            case Variable::Type::INT8:
                typestr = "int8  ";
                break;
            case Variable::Type::UINT16:
                typestr = "uint16";
                break;
            case Variable::Type::INT16:
                typestr = "int16 ";
                break;
            case Variable::Type::FLOAT:
                typestr = "float ";
                break;
        }

        std::cout << "    " << typestr << " " << var.name();
        auto it = defaults.find(var.name());
        if (it != defaults.end()) {
            std::cout << " = ";
            if (var.type() == Variable::Type::FLOAT) {
                std::cout << it->second.getFloat();
            } else {
                std::cout << it->second.getInt();
            }
        }
        std::cout << " (base_register = " << (int) var.startReg() << ")" << std::endl;
    }
}

static void showInfo(std::shared_ptr<DeviceInfo> info) {
    std::cout << "Name: " << info->name() << std::endl;
    std::cout << "Type: " << info->type() << std::endl;

    std::cout << "Writable variables:" << std::endl;
    showVars(info->writeVariables(), info->writeDefaults());
    std::cout << std::endl;

    for (auto group : info->pollGroups()) {
        std::cout << group.first << "ms Readable group:" << std::endl;
        showVars(group.second, info->readDefaults());
        std::cout << std::endl;
    }
}

class DumpInfo {
    private:
        Manager m_mgr;
        std::condition_variable m_cond;
        std::mutex m_lck;
        std::shared_ptr<DeviceInfo> m_info;

        std::shared_ptr<DeviceCallbacks> onConnected(Manager*, std::shared_ptr<DeviceInfo> info) {
            std::lock_guard<std::mutex> lock(m_lck);
            m_info = info;
            m_cond.notify_all();
            return nullptr;
        }

        void onPortFailed(Manager*, std::string msg) {
            std::cout << "Port error: " << msg << std::endl;
            m_cond.notify_all();
        }
    public:
        DumpInfo(const std::string& port) : 
            m_mgr(port,
                  std::bind(&DumpInfo::onConnected, this, _1, _2),
                  std::bind(&DumpInfo::onPortFailed, this, _1, _2))
        {
        }
        
        int run() {
            std::unique_lock<std::mutex> lock(m_lck);
            m_mgr.start();
            if (m_cond.wait_for(lock, std::chrono::seconds(2)) == std::cv_status::timeout) {
                std::cout << "No device found" << std::endl;
                return 0;
            } else if (m_info) {
                // just in case, unlock to allow more onConnected callbacks
                lock.unlock();
                m_mgr.stop();
                
                // Print out device info
                showInfo(m_info);
                return 0;
            } else {
                lock.unlock();
                m_mgr.stop();
                return 1;
            }
        }

};

static int info_run(std::map<std::string, std::string> args) {
    try {
        DumpInfo cmd(args["port"]);
        cmd.run();
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(info, "Report information on a connected device", info_run)
ARG_REQUIRED(port, "The device's port")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
