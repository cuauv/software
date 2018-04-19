#include <iostream>
#include <chrono>

#include <can/lib/Message.h>
#include <can/lib/CANSocket.h>

void usage() {
    std::cout << "Usage: sendParam intf devid param value" << std::endl;
}

int main(int argc, char *argv[]) {
    if (argc < 5) {
        usage();
        return -1;
    }

    uint8_t devid;
    uint8_t param;
    int16_t value;

    try {
        devid = std::stoul(argv[2]);
        param = std::stoul(argv[3]);
        value = std::stoul(argv[4]);
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
        MessageData paramData;
        paramData.int16Push(value);
        Message msg(Message::Type::PARAM,
                              0,
                              Selector::DevID(devid),
                              false,
                              param,
                              false,
                              paramData);
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
                            && msg->getType() == Message::Type::PARAM
                            && msg->getPayload() == param
                            && msg->getSelector() == Selector::DevID(0)) {
                        auto data = msg->getData();
                        int16_t retVal = data.int16Pop();
                        if (value != retVal) {
                            std::cout << "Parameter write failed" << std::endl;
                            return 2;
                        } else {
                            std::cout << "Parameter written successfully" << std::endl;
                            return 0;
                        }
                    }
                }
            } catch (std::exception& e) {
                std::cout << "[WARN]: " << e.what() << std::endl;
            }
        }
        std::cout << "Parameter write timed out" << std::endl;
        return 2;
    } catch (std::exception& e) {
        std::cout << "[ERR] " << e.what() << std::endl;
    }

    return 1;
}
