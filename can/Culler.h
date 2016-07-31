#pragma once

#include "Thread.h"
#include "Registry.h"

class Culler : public Thread {
    public:
        Culler(std::shared_ptr<Registry> registry);

    private:
        virtual void run();

        const std::shared_ptr<Registry> m_registry;
};
