// Lists the processes using shared memory


#include <stdio.h>

#include "libshm/c/shm.h"
#include "libshm/c/checksum.h"


void each_user(void(*f)(int)) {
    sem_t* sem = auv_shm_lock();

    // Check if the shm block is valid.
    int shmid = auv_shm_open(sizeof(struct shm_header), 0666);
    if (shmid < 0) {
        puts("Can't connect to shared memory for reasons unknown");
        sem_post(sem);
        abort();
    }

    struct shm_header* header = (struct shm_header*)shmat(shmid, 0, 0);
    printf("Connected to shared memory: %s\n", header->checksum);

    if (strcmp(header->checksum, SHM_CHECKSUM)) {
        printf("OUT OF DATE\n");
    }

    if (shm_header_empty(header)) {
        puts("No one is currently connected to shared memory.");
    }

    for (int i = 0; i < SHM_MAX_USERS; i++) {
        if (header->users[i] != 0) {
            f(header->users[i]);
        }
    }
    sem_post(sem);
}
