#include <iostream>
#include <fstream>
#include <functional>
#include <condition_variable>
#include <vector>
#include <algorithm>

#include <serial/libserial/Manager.h>
#include <serial/libserial/SerialPort.h>

#include "loader/MemoryImage.h"
#include "loader/AUVFirmware.h"
#include "loader/auvfw.h"

#include "BootTalker.h"

#include "../Command.h"

#define NUM_RETRY 3
#define TIMEOUT 500

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

        std::shared_ptr<DeviceCallbacks> onConnected(Manager*, std::shared_ptr<DeviceInfo>) {
            std::lock_guard<std::mutex> lock(m_lck);
            m_deviceFound = true;
            return std::make_shared<DummyCallbacks>(this);
        }

        void onPortFailed(Manager*, std::string msg) {
            std::cout << "Error: " << msg << std::endl;
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
            m_mgr.start();
            if (m_cond.wait_for(lock, std::chrono::seconds(2)) == std::cv_status::timeout) {
                return 0;
            } else if (m_deviceFound) {
                std::cout << "Resetting device..." << std::endl;
                m_mgr.resetDevice();
                return 0;
            } else {
                return 1;
            }
        }

};

static void flash_page(BootTalker &talker, uint16_t page_size, uint16_t addr, std::vector<uint8_t> bytes) {
        std::vector<uint8_t> buf;
        buf.push_back('U');
        buf.push_back((addr/2) & 0xFF);
        buf.push_back((addr/2) >> 8);
        talker.query(buf, TIMEOUT, 0);
        buf.clear();
        buf.push_back('d');
        // NOTE: not a bug here, the two commands do in fact switch endianness
        buf.push_back((page_size*2) >> 8);
        buf.push_back((page_size*2) & 0xFF);
        buf.push_back('F');
        buf.insert(buf.end(), bytes.begin(), bytes.end());
        talker.query(buf, TIMEOUT, 0);
}

static int flash_run(std::map<std::string, std::string> args) {
    std::string port;
    if (args.count("port") == 0) {
        auto ports = Manager::listPorts();
        if (ports.size() == 1) {
            port = *ports.begin();
        } else if (ports.size() > 1) {
            std::cout << "Multiple ports found, please specify one on the command line" << std::endl;
            return 1;
        } else {
            std::cout << "No ports found, aborting" << std::endl;
            return 1;
        }
    } else {
        port = args["port"];
    }

    try {
        std::ifstream fwin(args["fw"]);
        AUVFirmware fw = loadAUVfw(fwin);

        auto pages = fw.mem().asPages(fw.pageSize()*2);
        // Perform a remote reset first
        {
            // Drop in scope so that the serial port is released after the reset
            ResetDevice rset(port);
            if (rset.run() != 0) {
                std::cout << "Reset failed, aborting" << std::endl;
                return 1;
            }
        }

        // This delay is critical for some reason. It only is necessary when connecting
        // via the serial board, not through one of the Prolific adapters. It seems like
        // there's some extra data on the line when the serial board is talking, this sleep
        // gives the firmware enough time to time out whatever is confusing it

        std::this_thread::sleep_for(std::chrono::milliseconds(250));

        {
            BootTalker talker(std::make_unique<SerialPort>(port));
            std::cout << "Verifying device..." << std::endl;

            // Check the device name
            // Confirms that we are talking to the board the firmware is built for
            auto resp = talker.query({'i'}, TIMEOUT);
            std::string devName(resp.begin(), resp.end());
            if (devName != fw.deviceName()) {
                std::cout << "Error! Device name mismatch!" << std::endl;
                std::cout << "Expecting " << fw.deviceName() << ", found " << devName << std::endl;
                return 1;
            }

            // Check the device signature
            // Confirms that we are talking to the model of chip this firmware is built for
            resp = talker.query({'u'}, TIMEOUT, 3);
            if (resp[0] != fw.sig1()) {
                std::cout << "Error! SIG1 mismatch!" << std::endl;
                std::cout << "Expecting 0x" << std::hex << (int) fw.sig1() << ", got 0x" << (int) resp[0] << std::endl;
                return 1;
            } else if (resp[1] != fw.sig2()) {
                std::cout << "Error! SIG2 mismatch!" << std::endl;
                std::cout << "Expecting 0x" << std::hex << (int) fw.sig2() << ", got 0x" << (int) resp[1] << std::endl;
                return 1;
            } else if (resp[2] != fw.sig3()) {
                std::cout << "Error! SIG3 mismatch!" << std::endl;
                std::cout << "Expecting 0x" << std::hex << (int) fw.sig3() << ", got 0x" << (int) resp[2] << std::endl;
                return 1;
            }

            // Do the programming here
            std::cout << "Uploading code..." << std::endl;
            std::vector<uint8_t> ones_page(fw.pageSize() * 2);
            std::fill(ones_page.begin(), ones_page.end(), 0xff);
            flash_page(talker, fw.pageSize(), 0, ones_page);
            //std::cout << "cleared page 0" << std::endl;
            //std::cout << "pageSize = " << fw.pageSize() << std::endl;
            for (auto page : pages) {
                //std::cout << "page.size() = " << page.second.size() << std::endl;
                if (page.first != 0) flash_page(talker, fw.pageSize(), page.first, page.second);
                //std::cout << "wrote page " << page.first << std::endl;
            }
            flash_page(talker, fw.pageSize(), 0, pages[0]);

            // Read back pages
            std::cout << "Verifying code..." << std::endl;
            for (auto page : pages) {
                std::vector<uint8_t> buf;
                buf.push_back('U');
                buf.push_back((page.first/2) & 0xFF);
                buf.push_back((page.first/2) >> 8);
                talker.query(buf, TIMEOUT, 0);
                buf.clear();
                buf.push_back('t');
                // NOTE: not a bug here, the two commands do in fact switch endianness
                buf.push_back((fw.pageSize()*2) >> 8);
                buf.push_back((fw.pageSize()*2) & 0xFF);
                buf.push_back('F');
                auto resp = talker.query(buf, TIMEOUT, fw.pageSize()*2);
                if (resp != page.second) {
                    std::cout << "Error: page at address 0x" << std::hex << page.first <<
                        " failed to write" << std::endl;
                    return 1;
                }
            }
            
            // Send the final reset to launch user code
            std::cout << "Restarting device..." << std::endl;
            talker.query({'Q'}, TIMEOUT, 0);

            std::cout << std::endl;
            std::cout << "auvdude done.  Thank you." << std::endl;
        }
    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }

    return 0;
}

BEGIN_COMMAND_DEF(flash, "Flash a firmware onto a board", flash_run)
ARG_REQUIRED(fw, "The firmware to flash")
ARG_OPTIONAL(port, "The port the device is connected to")
END_COMMAND_DEF()

}}} // end namespace cuauv::serial::cli
