#include "remote.h"

#include "libshm/c/serialize.h"
#include "libshm/c/shm.h"

#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

// Treats the void* as an integer file descriptor.
void* handle(void* arg) {
    int* sock = (int*)arg;
    Serialize s(*sock);
    delete sock;

    s.WriteEverything();
    s.Handle(true);
    return NULL;
}

void Server() {
    shm_init();


    // Create the server.
    pthread_t server_thread;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&server_thread, &attr, Serialize::Serve, NULL);


    struct sockaddr_in* addr;
    struct sockaddr_in* client_addr;
    socklen_t client_addr_size;

    // Create socket
    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd == -1) {
        printf("Error creating socket: %d\n", errno);
    }

    // Allow address reuse
    int optval = 1;
    int err = setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(int));
    if (err == -1) {
        printf("Error setting SO_REUSEADDR on socket: %d\n", errno);
    }

    // bind
    addr = (struct sockaddr_in*)calloc(1, sizeof(struct sockaddr_in));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(11211); // htons: Convert to network byte order
    addr->sin_addr.s_addr = INADDR_ANY;
    err = bind(sock_fd, (struct sockaddr *)addr, sizeof(struct sockaddr));
    free(addr);
    if (err == -1) {
        printf("bind error: %d\n", errno);
    }

    err = listen(sock_fd, 1);
    if (err == -1) {
        printf("listen error: %d\n", errno);
    }

    client_addr = (struct sockaddr_in*)malloc(sizeof(struct sockaddr_in));
    client_addr_size = sizeof(struct sockaddr);
    pthread_t client_thread;

    while (true) {
        int conn = accept(sock_fd, (struct sockaddr *)client_addr, &client_addr_size);

        // Allocate a pointer so so we can pass it to a new thread.
        int* sock = new int(conn);

        pthread_create(&client_thread, &attr, handle, (void*)sock);
    }
}
