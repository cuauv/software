#ifndef CUAUV_SIM_UTIL_H
#define CUAUV_SIM_UTIL_H

#include <iostream>
#include <stdlib.h>

#define MALLOC_FAILED(where) \
    do { \
        std::cerr << #where ": Memory allocation failure. Terminating." << std::endl; \
        exit(1); \
    } while (0)

namespace cuauv {
namespace fishbowl {

enum class server_code {
    OK = 0,

    ERR_ALREADY_STARTED = 1,

    ERR_SOCKET_INIT = 2,
    ERR_SOCKET_ACCEPT = 3,

    ERR_POLL = 4,

    ERR_CLIENT_RECV = 5,
    ERR_CLIENT_MALFORMED = 6,
    ERR_CLIENT_EXCESSIVE_LEN = 7,
    ERR_CLIENT_SEND = 8,
    CLIENT_CLOSED = 9,
};

// (a, b)
// a is 0 on non-error
// on error, a is a [code] and b is a [std::string] message
typedef std::pair<server_code, std::string> server_rp;

typedef uint32_t client_id;

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_UTIL_H
