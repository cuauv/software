#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char *argv[])
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s <program> <arg> ...\n", argv[0]);
        exit(1);
    }

    // Set to a new process group so we don't receive the parent group's
    // signals (so the parent can do a clean shutdown).
    setpgrp();

    if (execvp(argv[1], argv + 1) == -1) {
        fprintf(stderr, "execvp: %s (%d)\n", strerror(errno), errno);
    }
}
