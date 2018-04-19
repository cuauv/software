#ifndef SHM_SHM_H
#define SHM_SHM_H

#include <semaphore.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <fcntl.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <pthread.h>
#include <stdbool.h>

#include "libshm/c/watcher.h"

#define SHM_MAX_USERS 1024
#define SHM_CHECKSUM_LEN 41

#ifdef __cplusplus
extern "C" {
#endif

struct shm_header {
    pid_t users[SHM_MAX_USERS];
    char checksum[SHM_CHECKSUM_LEN];
};

struct shm_meta {
    char f;
    char stream;
    pthread_mutex_t m;
    void *last_client;
    uint16_t w[MAX_WATCHERS];
};

<!--(for g in groups)-->
struct $!g['groupname']!$ {
    <!--(for k in g['varnames'])-->
        <!--(if g['vars'][k]['type'] == 'string')-->
    char $!k!$[$!g['vars'][k]['length']!$ + 1];
        <!--(else)-->
    $!g['vars'][k]['ctype']!$ $!k!$;
        <!--(end)-->
    <!--(end)-->
};
<!--(end)-->

struct shm {
    struct shm_header header;
    <!--(for g in groups)-->
    struct g_$!g['groupname']!$ {
        struct shm_meta m;
        struct $!g['groupname']!$ g;
		<!--(if sum(((g['vars'][k]['type'] == 'string') for k in g['varnames'])) > 0)-->
        struct $!g['groupname']!$_lens {
            <!--(for k in g['varnames'])-->
                <!--(if g['vars'][k]['type'] == 'string')-->
            size_t $!k!$;
                <!--(end)-->
            <!--(end)-->
        } l;
		<!--(end)-->
    } $!g['groupname']!$;
    <!--(end)-->
    struct watchers {
        char occupied[MAX_WATCHERS];
        struct var_watcher watchers[MAX_WATCHERS];
        pthread_mutex_t mut;
    } watch;
};

extern struct shm* shm;

void shm_strcpy(char* dst, const char* src, size_t length);

// Lock the shared memory file. Unlock it when done.
sem_t* auv_shm_lock();

// Get the shmid of the shm file of the given size with the given flags.
int auv_shm_open(size_t size, int flags);

// Open a connection to shared memory.
void shm_init();

// Broadcast to an array of watchers that a variable has changed.
void shm_notify(uint16_t* watchers);

// rm the shared mem file.
void shm_rm(int shmid);

// True if the header contains no valid pids.
bool shm_header_empty(struct shm_header* header);

void shm_set_defaults();

int _shm_lock(pthread_mutex_t *mut);

#define shm_lock(group) {                           \
    if (_shm_lock(&shm->group.m.m))                 \
        printf("shm: failed to lock '" #group "'.\n");  \
}

#define shm_unlock(group) {                         \
    if (pthread_mutex_unlock(&shm->group.m.m))      \
        printf("shm: failed to unlock '" #group "'.\n");  \
}

#define shm_get(group, name, location) {            \
shm_lock(group);                                    \
location = shm->group.g.name;                       \
shm_unlock(group);                                  \
}

#define shm_set(group, name, val) {                 \
shm_lock(group);                                    \
if ((val) != shm->group.g.name) {                   \
    shm->group.g.name = val;                        \
    shm->group.m.f = 1;                             \
    shm->group.m.stream = 1;                        \
    shm->group.m.last_client = 0;                   \
}                                                   \
shm_notify(shm->group.m.w);                         \
shm_unlock(group);                                  \
}

#define shm_get_nl(group, name, location) {            \
location = shm->group.g.name;                       \
}

#define shm_set_nl(group, name, val) {                 \
if ((val) != shm->group.g.name) {                   \
    shm->group.g.name = val;                        \
    shm->group.m.f = 1;                             \
    shm->group.m.stream = 1;                        \
    shm->group.m.last_client = 0;                   \
}                                                   \
}

#define shm_getstr(group, name, val) {              \
shm_lock(group);                                    \
shm_strcpy(val, shm->group.g.name, shm->group.l.name); \
shm_unlock(group);                                  \
}

#define shm_setstr(group, name, val) {              \
shm_lock(group);                                    \
shm_strcpy(shm->group.g.name, val, shm->group.l.name); \
shm->group.m.f = 1;                                 \
shm->group.m.stream = 1;                            \
shm->group.m.last_client = 0;                       \
shm_notify(shm->group.m.w);                         \
shm_unlock(group);                                  \
}

#define shm_getg(group, location) {                 \
shm_lock(group);                                    \
location = shm->group.g;                            \
shm_unlock(group);                                  \
}

#define shm_setg(group, val) {                      \
shm_lock(group);                                    \
shm->group.g = val;                                 \
shm->group.m.f = 1;                                 \
shm->group.m.stream = 1;                            \
shm->group.m.last_client = 0;                       \
shm_notify(shm->group.m.w);                         \
shm_unlock(group);                                  \
}

#define shm_zerog(group) {                          \
shm_lock(group);                                    \
memset(&shm->group.g, 0, sizeof(shm->group.g));     \
shm->group.m.f = 1;                                 \
shm->group.m.stream = 1;                            \
shm->group.m.last_client = 0;                       \
shm_notify(shm->group.m.w);                         \
shm_unlock(group);                                  \
}

/*
 * Watch |group| with |watcher|.
 */
#define shm_watch(group, watcher) {                 \
int w_id = WATCH_ID(watcher);                       \
if (w_id >= 0 && w_id < MAX_WATCHERS)               \
    shm->group.m.w[w_id] = WATCH_KEY(watcher);      \
}

#define shm_unwatch(group, watcher) {               \
int w_id = WATCH_ID(watcher);                       \
if (w_id >= 0 && w_id < MAX_WATCHERS)               \
    shm->group.m.w[w_id] = 0;                       \
}

#ifdef __cplusplus
}
#endif

#endif  // SHM_SHM_H
