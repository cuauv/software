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

    // Redirect stdout to /dev/null.
    int nullfd = open("/dev/null", O_WRONLY);
    assert(nullfd != -1);
    assert(dup2(nullfd, STDOUT_FILENO) != -1);

    if (execvp(argv[1], argv + 1) == -1) {
        fprintf(stderr, "execvp: %s (%d)\n", strerror(errno), errno);
    }
}
