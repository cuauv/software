#include "c/shm.h"

#include <stdio.h>
#include <time.h>
#include <unistd.h>

#define NUM_ITERS 10000000

// Returns the current time in nanoseconds.
uint64_t get_ns() {
    timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (uint64_t)ts.tv_sec * 1000000000LL + (uint64_t)ts.tv_nsec;
}

int main() {
    pid_t pid = fork();
    struct depth depth;
    struct desires desires;
    uint64_t start, end;

    shm_init();

    start = get_ns();

    for (int i = 0; i < NUM_ITERS; i++) {
        if (pid) {
            shm_getg(desires, desires);
            shm_setg(desires, desires);
        } else {
            shm_getg(depth, depth);
            shm_setg(depth, depth);
        }
    }

    end = get_ns();

    printf("%f\n", (end - start) / 1000000000.0);

    return 0;
}
