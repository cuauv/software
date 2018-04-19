#include "Thread.h"

Thread::Thread() :
            m_shouldTerminate(false)
{
}

Thread::~Thread() {
    shutdown();
}

void Thread::launch() {
    // Don't double launch and leak a thread
    if (m_thread.joinable()) return;

    m_shouldTerminate = false;
    m_thread = std::thread(Thread::internal_startup, this);
}

void Thread::shutdown() {
    // No thread running if not joinable
    if (!m_thread.joinable()) return;

    m_shouldTerminate = true;
    m_thread.join();
}

void Thread::internal_startup(Thread* self) {
    self->run();
}

bool Thread::shouldTerminate() {
    return m_shouldTerminate;
}
