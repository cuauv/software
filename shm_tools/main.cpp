#include <signal.h>
#include <stdio.h>
#include <string.h>

#include "libshm/c/shm.h"
#include "ps.h"
#include "remote.h"

void print(int pid);
void pkill(int pid);
void reset();

void usage(char* cmd) {
        printf("Usage: %s <command>\n", cmd);
        printf("Commands:\n");
        printf("\tps\t\tList processes using shared memory\n");
        printf("\tpkill\t\tKill processes using shared memory\n");
        printf("\treset\t\tReset all variables to their defaults\n");
        printf("\tRESET\t\tDeletes shm block. USE WITH CAUTION!\n");
        printf("\tserver\t\tStart a shared memory streaming server\n");
        printf("\tclient [server]\tConnect a shared memory streaming client to the sub\n");
}

int main(int argc, char** argv) {
    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "ps") == 0) {
        each_user(&print);
    } else if (strcmp(argv[1], "pkill") == 0) {
        each_user(&pkill);
    } else if (strcmp(argv[1], "reset") == 0) {
        reset();
    } else if (strcmp(argv[1], "RESET") == 0) {
        each_user(&pkill);
        shm_rm(auv_shm_open(sizeof(struct shm), 0));
        reset();
    } else if (strcmp(argv[1], "server") == 0) {
        Server();
    } else if (strcmp(argv[1], "client") == 0) {
        if (argc > 2) {
            Client(argv[2]);
        } else {
            Client((char*)"192.168.0.93");
        }
    } else {
        usage(argv[0]);
    }
}

void print(int pid) {
    printf("%d\n", pid);
}

void pkill(int pid) {
    kill(pid, SIGTERM);
    printf("Killing %d\n", pid);
}

void reset() {
    shm_init();
    shm_set_defaults();
}
