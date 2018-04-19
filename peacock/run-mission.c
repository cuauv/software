#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <zmq.h>

void msg_free(void* data, void* hint)
{
    free(data);
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        printf("Usage: %s <module> <task> [arg] ...\n", argv[0]);
        exit(1);
    }

    int rc;

    void* ctx = zmq_ctx_new();
    assert(ctx != NULL);
    void* socket = zmq_socket(ctx, ZMQ_REQ);
    assert(socket != NULL);
    rc = zmq_connect(socket, "tcp://127.0.0.1:6666");
    assert(rc == 0);

    int len = 0;
    len += strlen(argv[1]) + 1;
    len += strlen(argv[2]) + argc > 3 ? 1 : 0;
    for (int i = 3; i < argc; i++) {
        len += strlen(argv[i]);
        if (i + 1 < argc) {
            len++;
        }
    } 

    char* data = malloc(len);
    assert(data != NULL);

    size_t at = 0;
    for (int i = 1; i < argc; i++) {
        strcpy(data + at, argv[i]);
        at += strlen(argv[i]);
        if (i + 1 < argc) {
            data[at] = ' ';
            at++;
        }
    }

    zmq_msg_t msg;
    rc = zmq_msg_init_data(&msg, data, len, msg_free, NULL);
    assert (rc == 0);

    rc = zmq_msg_send(&msg, socket, 0);
    if (rc == -1) {
        fprintf(stderr, "zmq_msg_send: %s (%d)\n", zmq_strerror(errno), errno);
        return 1;
    }

    rc = zmq_msg_recv(&msg, socket, 0);
    if (rc == -1) {
        fprintf(stderr, "zmq_msg_recv: %s (%d)\n", zmq_strerror(errno), errno);
        return 1;
    }

    printf("%.*s\n", zmq_msg_size(&msg), zmq_msg_data(&msg));

    zmq_close(socket);
    zmq_term(ctx);

    return 0;
}
