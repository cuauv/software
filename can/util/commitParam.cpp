#include <iostream>
#include <chrono>

#include <can/lib/Message.h>
#include <can/lib/CANSocket.h>

void usage() {
    std::cout << "Usage: commitParam intf devid param" << std::endl;
}

int main(int argc, char *argv[]) {
    if (argc < 4) {
        usage();
        return -1;
    }

    uint8_t devid;
    uint8_t param;

    try {
        devid = std::stoul(argv[2]);
        param = std::stoul(argv[3]);
    } catch (std::exception& e) {
        std::cout << "Non-integer argument passed" << std::endl;
        return 1;
    }
    
    if (devid > 63) {
        std::cout << "Invalid device id" << std::endl;
        return 1;
    }

    try {
        CANSocket sock(argv[1]);
        Message msg(Message::Type::COMMIT,
                              0,
                              Selector::DevID(devid),
                              false,
                              param,
                              false,
                              {});
        sock.sendMessage(msg);

        auto end_time = std::chrono::steady_clock::now() + std::chrono::milliseconds(250);
        while (end_time > std::chrono::steady_clock::now()) {
            struct timeval timeout;
            timeout.tv_sec = 0;
            timeout.tv_usec = 250000;
            
            try {
                auto msg = sock.readMessage(&timeout);
                if (msg) {
                    if (msg->getSource() == devid
                            && msg->getType() == Message::Type::COMMIT
                            && msg->getPayload() == param
                            && msg->getSelector() == Selector::DevID(0)) {
                        std::cout << "Parameter committed successfully" << std::endl;
                        return 0;
                    }
                }
            } catch (std::exception& e) {
                std::cout << "[WARN]: " << e.what() << std::endl;
            }
        }
        std::cout << "Parameter commit timed out" << std::endl;
        return 2;
    } catch (std::exception& e) {
        std::cout << "[ERR] " << e.what() << std::endl;
    }

    return 1;
}
