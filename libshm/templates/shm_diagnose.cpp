#include "shm.h"

int lock(pthread_mutex_t* m) {
    for (int i = 0; i < 3; i++)
        if (!pthread_mutex_trylock(m))
            return 1;
    return 0;
}

int count_watchers(uint16_t* watchers) {
    int c = 0;
    for (int i = 0; i < MAX_WATCHERS; i++)
        c += (watchers[i] != 0);
    return c;
}

int print_abandoned(watcher_t w) {
    printf("Watcher 0x%x: is abandoned.\n", w);
    return 0;
}

int main(int argc, char** argv) {
    shm_init();

    if (lock(&shm->watch.mut)) {
        for (int i = 0; i < MAX_WATCHERS; i++) {
            if (shm->watch.occupied[i]) {
                struct var_watcher* w = &shm->watch.watchers[i];
                printf("Watcher: 0x%x (pid %d)\n", WATCHER(i, w->key), w->owner);
            }
        }
        pthread_mutex_unlock(&shm->watch.mut);
        collect_abandoned_watchers(print_abandoned);
    } else {
        printf("Watcher list is locked!\n");
    }

    pthread_mutex_t* m;
    <!--(for g in groups)-->
    m = &shm->$!g['groupname']!$.m.m;
    if (lock(m)) {
        int w = count_watchers(shm->$!g['groupname']!$.m.w);
        if (w) {
            printf("Group $!g['groupname']!$ has %d watchers.\n", w);
        }
        pthread_mutex_unlock(m);
    } else {
        printf("Group $!g['groupname']!$ is locked.\n");
    }
    <!--(end)-->

    return 0;
}
