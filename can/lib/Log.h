#pragma once

#include <list>
#include <string>

class Log {
    public:
        static void init(size_t numMsgs, size_t msgLen);

        static void log(std::string msg);

        static std::list<std::string> getMessages();
};
