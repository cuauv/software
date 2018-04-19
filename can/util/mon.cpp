#include <iostream>

#include <can/lib/Message.h>
#include <can/lib/CANSocket.h>

void usage() {
    std::cout << "Usage: mon intf" << std::endl;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        usage();
        return -1;
    }

    try {
        CANSocket sock(argv[1]);
        while (1) {
            try {
                auto msg = sock.readMessage(NULL);
                if (msg) {
                    if (msg->getSource() == 0)
                        std::cout << "[HOST] ";
                    else
                        std::cout << "[DEV] ";
                    std::cout << msg->str() << std::endl;
                } else {
                    std::cout << "[ERR] UNEXPECTED NULL MESSAGE" << std::endl;
                }
            } catch (std::exception& e) {
                std::cout << "[WARN] " << e.what() << std::endl;
            }
        }
    } catch (std::exception& e) {
        std::cout << "[ERR] " << e.what() << std::endl;
    }

    return 0;
}
