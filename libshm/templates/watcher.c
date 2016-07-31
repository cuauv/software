#include "watcher.h"

#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#include "shm.h"

#define INVALID(wid) printf("shm: watcher %d is not valid!\n", wid)

#define LOCK_ERR(location) printf("shm: AAAAAAHHHH failed to lock or unlock something in '" #location "()'!!!\n")

#define FAIL(wid) {                                 \
    INVALID(wid);                                   \
    return -1;                                      \
}

#define LOCK_FAIL(location) {                       \
    LOCK_ERR(location);                             \
    return -1;                                      \
}

watcher_t create_watcher() {
    if (_shm_lock(&shm->watch.mut))
        LOCK_FAIL(create_watcher);

    // Periodic destruction of abandoned watchers
    collect_abandoned_watchers(destroy_watcher);

    int i;
    // Find first empty slot
    for(i=0; i<MAX_WATCHERS && shm->watch.occupied[i]; i++)
        ;

    // Out of watchers, garbage collect and try again.
    if(i==MAX_WATCHERS) {
        puts("Out of watchers! Can't create another.");
        if (pthread_mutex_unlock(&shm->watch.mut))
            LOCK_FAIL(create_watcher);
        return -1;
    }

    struct var_watcher* new_watcher = &shm->watch.watchers[i];

    new_watcher->key++;
    if (new_watcher->key == 0) {
        new_watcher->key++;
    }

    new_watcher->owner = getpid();
    new_watcher->modified = 0;
    shm->watch.occupied[i] = 1;

    if (pthread_mutex_unlock(&shm->watch.mut))
        LOCK_FAIL(create_watcher);

    return WATCHER(i, new_watcher->key);
}

int destroy_watcher(watcher_t watcher) {
    int id = WATCH_ID(watcher);
    int key = WATCH_KEY(watcher);

    if (id < 0 || id > MAX_WATCHERS || !shm->watch.occupied[id])
        FAIL(watcher);

    struct var_watcher* w = &shm->watch.watchers[id];

    // This is a reference to a dead watcher.
    if (w->key != key) {
        FAIL(watcher);
    }

    shm->watch.occupied[id] = 0;
    
    return 0;
}

int wait_watcher(watcher_t watcher, bool new_update) {
    int id = WATCH_ID(watcher);
    int key = WATCH_KEY(watcher);

    if (id < 0 || id > MAX_WATCHERS || !shm->watch.occupied[id])
        FAIL(watcher);

    struct var_watcher* w = &shm->watch.watchers[id];

    // This is a reference to a dead watcher.
    if (w->key != key) {
        FAIL(watcher);
    }

    if (_shm_lock(&w->mutex))
        LOCK_FAIL(wait_watcher);

    // clear modified flag before waiting if we want a new update
    if (new_update) {
        w->modified = w->modified > 1 ? w->modified : 0;
    }

    while (!w->modified)
        pthread_cond_wait(&w->condition, &w->mutex);

    w->modified = w->modified > 1 ? w->modified : 0;
    if (pthread_mutex_unlock(&w->mutex))
        LOCK_FAIL(wait_watcher);
    return 0;
}

int watcher_has_changed(watcher_t watcher) {
    int id = WATCH_ID(watcher);
    int key = WATCH_KEY(watcher);

    if (id < 0 || id > MAX_WATCHERS || !shm->watch.occupied[id])
        FAIL(watcher);

    struct var_watcher* w = &shm->watch.watchers[id];

    // This is a reference to a dead watcher.
    if (w->key != key) {
        FAIL(watcher);
    }

    if (_shm_lock(&w->mutex)) {
        LOCK_ERR(has_changed);
        return 0;
    }

    // Clear the modified flag unless we've been permanently unblocked.
    int modified = w->modified;
    w->modified = modified > 1 ? modified : 0;
    if (pthread_mutex_unlock(&w->mutex)) {
        LOCK_ERR(has_changed);
        return 0;
    }

    return modified;
}

/*
 * Set this watcher's modified flag to |flag|.
 * Return nonzero on failure.
 */
int set_modified(watcher_t watcher, char flag) {
    int id = WATCH_ID(watcher);
    int key = WATCH_KEY(watcher);

    if (id < 0 || id > MAX_WATCHERS || !shm->watch.occupied[id])
        FAIL(watcher);

    struct var_watcher* w = &shm->watch.watchers[id];

    // This is a reference to a dead watcher.
    if (w->key != key) {
        return -1;
    }
    
    if (_shm_lock(&w->mutex))
        LOCK_FAIL(broadcast);

    // Only set the flag if not permanently disabled
    w->modified = w->modified > 1 ? w->modified : flag;
    pthread_cond_broadcast(&w->condition);
    
    if (pthread_mutex_unlock(&w->mutex))
        LOCK_FAIL(broadcast);

    return 0;
}

/*
 * Wake any threads waiting on this watcher.
 * Return nonzero on failure.
 */
int broadcast_watcher(watcher_t watcher) {
    return set_modified(watcher, 1);
}

/*
 * Permanently disable this watcher so that calls to wait() do not block.
 */
int disable_watcher(watcher_t watcher) {
    return set_modified(watcher, 2);
}

/*
 * Call f for each watcher that does not have a valid pid.
 */
void collect_abandoned_watchers(int(f)(watcher_t)) {
    for (int i=0; i<MAX_WATCHERS; i++) {
        if (shm->watch.occupied[i] && kill(shm->watch.watchers[i].owner, 0)) {
            f(WATCHER(i, shm->watch.watchers[i].key));
        }
    }
}
