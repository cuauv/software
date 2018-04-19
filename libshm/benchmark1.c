#include "c/shm.h"
#include "c/dynamic.h"

#include <stdio.h>
#include <time.h>

#define NUM_ITERS 10000000

// Returns the current time in nanoseconds.
uint64_t get_ns() {
    timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (uint64_t)ts.tv_sec * 1000000000LL + (uint64_t)ts.tv_nsec;
}

int main() {
    struct depth depth;
    uint64_t start, end;
    shm_init();

    start = get_ns();

    for (int i = 0; i < NUM_ITERS; i++) {
        shm_getg(depth, depth);
        shm_setg(depth, depth);
    }

    end = get_ns();

    printf("Static: %f\n", (end - start) / 1000000000.0);

    start = get_ns();

    auto g = cuauv::dshm::newGroup("depth");
    for (int i = 0; i < NUM_ITERS; i++) {
        g->pull();
        g->push();
    }

    end = get_ns();

    printf("Dynamic (same group): %f\n", (end - start) / 1000000000.0);

    start = get_ns();

    for (int i = 0; i < NUM_ITERS; i++) {
        auto g = cuauv::dshm::newGroup("depth");
        g->pull();
        g->push();
    }

    end = get_ns();

    printf("Dynamic: %f\n", (end - start) / 1000000000.0);

    return 0;
}
