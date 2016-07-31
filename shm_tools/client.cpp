#include "remote.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 


#include "libshm/c/serialize.h"
#include "libshm/c/shm.h"

void error(const char *msg) {
    perror(msg);
    exit(0);
}

void Client(char* hostname) {
    shm_init();

    int sockfd, portno;
    struct sockaddr_in serv_addr;
    struct hostent *server;

    portno = 11211;
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) 
        error("ERROR opening socket");

    server = gethostbyname(hostname);
    if (server == NULL) {
        fprintf(stderr,"ERROR, no such host\n");
        exit(0);
    }
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *)server->h_addr, 
         (char *)&serv_addr.sin_addr.s_addr,
         server->h_length);
    serv_addr.sin_port = htons(portno);
    if (connect(sockfd,(struct sockaddr *) &serv_addr,sizeof(serv_addr)) < 0) 
        error("ERROR connecting");
    puts("Connected.");


    Serialize s(sockfd);

    pthread_t server_thread;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&server_thread, &attr, Serialize::Serve, NULL);
    pthread_attr_destroy(&attr);

    s.Handle(false);
}
