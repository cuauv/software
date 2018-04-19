#include <iostream>
#include <chrono>

#include <can/lib/Message.h>
#include <can/lib/CANSocket.h>

void usage() {
    std::cout << "Usage: reset intf devid" << std::endl;
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        usage();
        return -1;
    }

    uint8_t devid;

    try {
        devid = std::stoul(argv[2]);
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
        Message msg(Message::Type::RESET,
                              0,
                              Selector::DevID(devid),
                              false,
                              0,
                              false,
                              {});
        sock.sendMessage(msg);

        std::cout << "Reset message sent to devid " << devid << std::endl;
        return 0;
    } catch (std::exception& e) {
        std::cout << "[ERR] " << e.what() << std::endl;
    }

    return 1;
}
