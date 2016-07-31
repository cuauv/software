#include "Culler.h"

#include <iostream>

Culler::Culler(std::shared_ptr<Registry> registry) : 
                    m_registry(registry)
{
}

void Culler::run() {
    while (!shouldTerminate()) {
        auto loop_start = std::chrono::steady_clock::now();

        m_registry->cull();

        std::this_thread::sleep_until(loop_start + std::chrono::seconds(1));
    }
}
