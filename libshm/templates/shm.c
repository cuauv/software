#include "shm.h"

#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <stdbool.h>
#include <gnu/libc-version.h>

#include "libshm/c/checksum.h"

struct shm* shm;

const key_t SHM_KEY = 0x617576; //auv
const char SHM_SEM_KEY[] = "/auv_shared_mem";

// Initialize all the variables.
void shm_init_vars();
// Append the current pid to the shm file.
void connect(struct shm_header* header);

void shm_strcpy(char* dst, const char* src, size_t length) {
    for (; *src != '\0' && length > 1; length--) {
        *(dst++) = *(src++);
    }
    *dst = '\0';
}

sem_t* auv_shm_lock() {
    sem_t* sem = sem_open(SHM_SEM_KEY, O_CREAT, 0666, 1);
    if (sem == SEM_FAILED) {
      puts("Could not access shm semaphore. Check permissions in /dev/shm.");
      abort();
    }

    sem_wait(sem);
    return sem;
}

int auv_shm_open(size_t size, int flags) {
    return shmget(SHM_KEY, size, flags);
}

int _shm_lock(pthread_mutex_t *mut) {
    int ret = pthread_mutex_lock(mut);
    if (ret == EOWNERDEAD) {
        return pthread_mutex_consistent(mut);
    }

    return ret;
}

void shm_init() {
    sem_t* sem = auv_shm_lock();
    bool create = false;

    // Check if the shm block is valid.
    int shmid = auv_shm_open(sizeof(struct shm_header), 0666);
    if (shmid >= 0) {
        struct shm_header* header = (struct shm_header*)shmat(shmid, 0, 0);
        bool checksum = strcmp(header->checksum, SHM_CHECKSUM) == 0;
        bool empty = shm_header_empty(header);
        shmdt(header);

        if (!checksum) {
            if (empty) {
                shm_rm(shmid);
                create = true;
            } else {
                sem_post(sem);
                puts("shm versions do not match. Please restart all shm clients.");
                abort();
            }
        }
    } else {
        create = true;
    }

    // Connect to shared memory.
    shmid = auv_shm_open(sizeof(struct shm), IPC_CREAT|0666);
    shm = (struct shm*)shmat(shmid, 0, 0);
    if (shm <= 0) {
        shm_rm(shmid);
        sem_post(sem);
        puts("Failed to connect to shared memory.");
        abort();
    }

    // Initialize everything if we created a new file.
    if (create) {
        shm_init_vars();
    }

    // Mark ourselves as using the shm file.
    connect(&shm->header);

    sem_post(sem);
}

void shm_rm(int shmid) {
    shmctl(shmid, IPC_RMID, NULL);
    return;
}

void connect(struct shm_header* header) {
    for (int i = 0; i < SHM_MAX_USERS; i++) {
        if (header->users[i] == 0) {
            header->users[i] = getpid();
            return;
        }
    }
    puts("Too many users attached to shm file!");
    abort();
}

bool shm_header_empty(struct shm_header* header) {
    // Remove invalid pids.
    for (int i = 0; i < SHM_MAX_USERS; i++) {
        if (header->users[i] != 0) {
            if (kill(header->users[i], 0)) {
                header->users[i] = 0;
            }
        }
    }

    for (int i = 0; i < SHM_MAX_USERS; i++) {
        if (header->users[i] != 0) {
            return false;
        }
    }
    return true;
}

void shm_set_defaults() {
    <!--(for g in groups)-->
        <!--(for k in g['varnames'])-->
            <!--(if g['vars'][k]['type'] == 'string')-->
                <!--(if 'default' in g['vars'][k])-->
    shm_setstr($!g['groupname']!$, $!k!$, $!g['vars'][k]['default']!$);
                <!--(else)-->
    shm_setstr($!g['groupname']!$, $!k!$, "");
                <!--(end)-->
            <!--(else)-->
                <!--(if 'default' in g['vars'][k])-->
    shm_set($!g['groupname']!$, $!k!$, $!g['vars'][k]['default']!$);
                <!--(else)-->
    shm_set($!g['groupname']!$, $!k!$, 0);
                <!--(end)-->
            <!--(end)-->
        <!--(end)-->
    <!--(end)-->
}

void shm_init_vars() {
    pthread_mutexattr_t mattr;
    pthread_mutexattr_init(&mattr);
    pthread_mutexattr_setpshared(&mattr, PTHREAD_PROCESS_SHARED);
    pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ERRORCHECK);
    // If a process dies while holding a shm lock. We don't want to freeze shm.
	
	// TODO handle robust mutex malfunctions in newer glibc version
	if (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 25)) {
		pthread_mutexattr_setrobust(&mattr, PTHREAD_MUTEX_ROBUST);
	}

    pthread_condattr_t cattr;
    pthread_condattr_init(&cattr);
    pthread_condattr_setpshared(&cattr, PTHREAD_PROCESS_SHARED);

    // Set header.
    memset(&shm->header, 0,sizeof(shm->header));
    strcpy(shm->header.checksum, SHM_CHECKSUM);

    // Initialize mutices and string lengths.
    <!--(for g in groups)-->
    pthread_mutex_init(&shm->$!g['groupname']!$.m.m, &mattr);
    shm->$!g['groupname']!$.m.f = false;
    shm->$!g['groupname']!$.m.stream = false;
    shm->$!g['groupname']!$.m.last_client = 0;
        <!--(for k in g['varnames'])-->
            <!--(if g['vars'][k]['type'] == 'string')-->
    shm->$!g['groupname']!$.l.$!k!$ = $!g['vars'][k]['length']!$ + 1;
            <!--(end)-->
        <!--(end)-->
    <!--(end)-->

    // Initialize watchers.
    pthread_mutex_init(&shm->watch.mut, &mattr);
    for (int i = 0; i < MAX_WATCHERS; i++) {
        pthread_mutex_init(&shm->watch.watchers[i].mutex, &mattr);
        pthread_cond_init(&shm->watch.watchers[i].condition, &cattr);
    }

    pthread_mutexattr_destroy(&mattr);
    pthread_condattr_destroy(&cattr);

    // Initialize the vars themselves.
    shm_set_defaults();
}

void shm_notify(uint16_t* watchers) {
    // Try to notify each attached watcher.
    // If we fail, assume future failure and remove
    // the watcher from this list.
    for (int i = 0; i < MAX_WATCHERS; i++) {
        if (watchers[i] && (!shm->watch.occupied[i] ||
            broadcast_watcher(WATCHER(i, watchers[i]))))
                watchers[i] = 0;
    }
}
