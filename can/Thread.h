#pragma once
#include <thread>

class Thread {
    public:
        Thread();
        ~Thread();
        void launch();
        void shutdown();

    protected:
        bool shouldTerminate();

    private:
        virtual void run() = 0;

        static void internal_startup(Thread* self);

        std::thread m_thread;
        bool m_shouldTerminate;
};
