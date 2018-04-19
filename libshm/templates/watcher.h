#ifndef SHM_WATCHER_H
#define SHM_WATCHER_H

#include <sys/types.h>
#include <stdint.h>

#define MAX_WATCHERS 255

#define WATCH_ID(w) ((w) >> 16)
#define WATCH_KEY(w) ((w) & 0xffff)

#define WATCHER(id, key) (((id) << 16) | (key))

#ifdef __cplusplus
extern "C" {
#endif

struct var_watcher {
    pthread_cond_t condition;
    pthread_mutex_t mutex;
    pid_t owner;
    char modified;
    uint16_t key;
};

// Lower 16 bits are a rotating key.
// Upper 16 are the id.
typedef int32_t watcher_t;

// Create a new watcher.
// Returns negative if we're out of watchers.
watcher_t create_watcher();

/*
 * Destroy the given watcher.
 * Returns nonzero if the watcher didn't exist to begin with.
 */
int destroy_watcher(watcher_t watcher);

/*
 * Block the current thread until some variable watched
 * by the given watcher is updated.
 * If new_update is false, wait_watcher may return immediately
 * if a variable was updated since creation / last wait
 * Returns nonzero on failure.
 */
int wait_watcher(watcher_t watcher, bool new_update);

/*
 * Returns nonzero if a watched variable has been modified and
 * resets the modified flag.
 */
int watcher_has_changed(watcher_t watcher);

/*
 * Wake any processes/threads waiting on this watcher.
 * Returns nonzero on failure.
 */
int broadcast_watcher(watcher_t watcher);

/*
 * Permanently disable this watcher so that calls to wait() do not block.
 * Will wake the watcher if it is currently waiting.
 */
int disable_watcher(watcher_t watcher);

/*
 * Call f for each watcher that does not have a valid pid.
 */
void collect_abandoned_watchers(int(f)(watcher_t));

#ifdef __cplusplus
}
#endif

#endif  // SHM_WATCHER_H
