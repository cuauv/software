#include "Log.h"

#include <mutex>

static std::list<std::string> s_messages;
static std::mutex s_lock;
static size_t s_numMsgs;
static size_t s_msgLen;

void Log::init(size_t numMsgs, size_t msgLen) {
    std::lock_guard<std::mutex> lck(s_lock);
    s_numMsgs = numMsgs;
    s_msgLen = msgLen;
    s_messages.clear();
    s_messages.resize(numMsgs, std::string(msgLen, ' '));
}

void Log::log(std::string msg) {
    std::lock_guard<std::mutex> lck(s_lock);

    msg.resize(s_msgLen, ' ');
    s_messages.pop_back();
    s_messages.push_front(msg);
}

std::list<std::string> Log::getMessages() {
    std::lock_guard<std::mutex> lck(s_lock);

    return s_messages;
}
