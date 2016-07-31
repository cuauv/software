#ifndef CUAUV_SIM_SERVER_H
#define CUAUV_SIM_SERVER_H

#include <memory>
#include <unordered_map>
#include <mutex>

#include <netdb.h>
#include <poll.h>

#include "spdlog/spdlog.h"

#include "util.hpp"
#include "protocol.hpp"
#include "bitreader.hpp"
#include "bitwriter.hpp"

namespace cuauv {
namespace fishbowl {

const int CLIENT_BUFFER_LEN = 1024;

enum class client_state {
    RECV_LENGTH,
    RECV_TYPE,
    RECV_BODY
};

struct client {
    int fd;

    client_state state = client_state::RECV_TYPE;

    uint8_t* buff; //!< The receive buffer.
    uint32_t i; //!< The next position to write to in the buffer.
    /**
     * The total number of bytes we expect to have in the buffer by the end of
     * the RECV_BODY state. 
     */
    uint32_t len;
    uint8_t type;

    bool remove = false;
};

class server {
public:
    server(protocol<bitreader, bitwriter>& proto);

    server_rp serve(int port);

    void open(client_id id, std::function<void(bitwriter&, uint8_t&)> f);

    void shutdown();

    std::shared_ptr<spdlog::logger> log;

private:
    server_rp serve_init(int port);
    server_rp handle(client_id id);

    void remove_client(client_id id);

    protocol<bitreader, bitwriter>& proto;

    std::mutex mutex;
    bool running = false;
    int listener = -1;
    std::vector<client_id> poll_ids;
    std::unordered_map<client_id, client> clients;
    client_id next_client_id = 0;
};

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_SERVER_H
