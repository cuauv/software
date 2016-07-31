#include <utility>
#include <functional>
#include <stdlib.h>
#include <unordered_map>
#include <stdio.h>
#include <mutex>

#include <endian.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <poll.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>

#include "server.hpp"
#include "util.hpp"
#include "protocol.hpp"
#include "fmts.hpp"
#include "bits.hpp"
#include "bitreader.hpp"
#include "bitwriter.hpp"

#define RET_ON_ERR(expr, err, name) \
    do { \
        if ((expr) == -1) { \
            return std::make_pair(err, fmts(#name ": %s (%d)", strerror(errno), errno)); \
        } \
    } while (0);

#define LOG(expr) \
    do { \
        if (log.get() != nullptr) { \
            log->expr; \
        } \
    } while (0);

#define ATTEMPT_RECV(n) \
    do { \
        len = recv(c.fd, c.buff, n, 0); \
        if (len == 0) { \
            return { server_code::CLIENT_CLOSED, "" }; \
        } \
        if (len == -1) { \
            if (errno == EINTR) { \
                return { server_code::OK, "" }; \
            } \
            return { server_code::ERR_CLIENT_RECV, fmts("recv: %s (%d)", strerror(errno), errno) }; \
        } \
        c.i += len; \
    } while (0);

#define SET_BUFF(len) \
    do { \
        c.buff = new uint8_t[len]; \
        if (c.buff == nullptr) { \
            MALLOC_FAILED(server::handle); \
        } \
        c.i = 0; \
    } while (0);

namespace cuauv {
namespace fishbowl {

static constexpr uint32_t MAX_CLIENT_MESSAGE_LEN = 1024;
static constexpr uint32_t MAX_CLIENT_STRING_LENGTH = 128;

server::server(protocol<bitreader, bitwriter>& proto)
    : proto(proto)
{
    using namespace std::placeholders;
    proto.open = std::bind(&server::open, this, _1, _2);
}

server_rp server::serve_init(int port)
{
    addrinfo *servinfo;
    int res;

    std::string ports = std::to_string(port);
    addrinfo hints {};

    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    if ((res = getaddrinfo(nullptr, ports.data(), &hints, &servinfo)) != 0) {
        return { server_code::ERR_SOCKET_INIT, fmts("getaddrinfo: %s (%d)", gai_strerror(res), res) };
    }

    RET_ON_ERR(
            listener = socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol),
            server_code::ERR_SOCKET_INIT, socket);

    int yes = 1;
    RET_ON_ERR(
            setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof (int)),
            server_code::ERR_SOCKET_INIT, setsockopt);

    RET_ON_ERR(
            bind(listener, servinfo->ai_addr, servinfo->ai_addrlen),
            server_code::ERR_SOCKET_INIT, bind);

    freeaddrinfo(servinfo);

    RET_ON_ERR(
            listen(listener, 10),
            server_code::ERR_SOCKET_INIT, listen);

    return { server_code::OK, "" };
}

void server::shutdown()
{
    std::unique_lock<std::mutex> lock(mutex);

    if (listener == -1) {
        return;
    }

    running = false;

    // shutdown instead of close so the polling will stop
    while (::shutdown(listener, SHUT_RDWR) == -1) {
        if (errno != EINTR) {
            close(listener);
            break;
        }
    }
}

// Assumes that the kernel send buffer is sufficiently large for the most
// sizes and tries to send the entire message in one go, repeating until
// [Σsend(...) = length].
static bool transmit(int fd, uint8_t* data, uint32_t length)
{
    uint32_t sent = 0;
    while (sent < length) {
        int res = send(fd, data + sent, length - sent, 0);
        if (res == -1) {
            if (errno == EINTR) {
                continue;
            }
            return false;
        }
        sent += res;
    }
    return true;
}

static bool respond(int fd, uint8_t type, uint8_t* data, uint32_t length)
{
    uint8_t header[5];
    header[0] = type;
    bytes_of_uint32(header + 1, length);
    if (!transmit(fd, header, 5)) {
        return false;
    }
    if (!transmit(fd, data, length)) {
        return false;
    }
    return true;
}

static server_rp handle_(protocol<bitreader, bitwriter>& proto, client_id id, client& c)
{
    server_rp res;
    bitreader rd(c.len == 0 ? nullptr : c.buff, c.len, MAX_CLIENT_STRING_LENGTH);
    bitwriter wt;
    uint8_t rtype;
    res = proto.handle(id, c.type, rd, wt, rtype);
    if (res.first != server_code::OK) {
        return res;
    }

    if (!respond(c.fd, rtype, wt.data(), wt.size())) {
        return { server_code::ERR_CLIENT_SEND, "Failed to send message to client." };
    }

    return {};
}

server_rp server::handle(client_id id)
{
    client& c = clients.at(id);
    int len;
    // The buffer must always be a malloc()ed section of memory,
    // even when not in use, so cleanup will not double free().
    switch (c.state) {
    case client_state::RECV_TYPE:
        // Not using ATTEMPT_RECV because we're only receiving one byte.
        // Might as well keep it on the stack.
        len = recv(c.fd, &c.type, 1, 0);
        if (len == 0) {
            return { server_code::CLIENT_CLOSED, "" };
        }
        if (len == -1) {
            if (errno == EINTR) {
                return { server_code::OK, "" };
            }
            return { server_code::ERR_CLIENT_RECV, fmts("recv: %s (%d)", strerror(errno), errno) };
        }
        delete[] c.buff;
        SET_BUFF(4);
        c.state = client_state::RECV_LENGTH;
        break;
    case client_state::RECV_LENGTH:
        ATTEMPT_RECV(4);
        if (c.i == 4) {
            c.state = client_state::RECV_TYPE;
            c.len = uint32_of_bytes(c.buff);
            if (c.len > MAX_CLIENT_MESSAGE_LEN) {
                return { server_code::ERR_CLIENT_EXCESSIVE_LEN, "" };
            }
            delete[] c.buff;
            if (c.len > 0) {
                SET_BUFF(c.len);
                c.state = client_state::RECV_BODY;
            } else {
                SET_BUFF(4);

                // 0-length message, handle directly then return to RECV_LENGTH
                // state
                server_rp res = handle_(proto, id, c);
                if (res.first != server_code::OK) {
                    return res;
                }

                c.state = client_state::RECV_TYPE;
            }
        }
        break;
    case client_state::RECV_BODY:
        ATTEMPT_RECV(c.len - c.i);

        if (c.i == c.len) {
            server_rp res = handle_(proto, id, c);
            if (res.first != server_code::OK) {
                return res;
            }

            c.state = client_state::RECV_TYPE;
        } 
        break;
    }

    return {};
}

void server::open(client_id id, std::function<void(bitwriter&, uint8_t&)> f)
{
    bitwriter wt;
    uint8_t rtype;
    f(wt, rtype);

    client& c = clients.at(id);

    if (!respond(c.fd, rtype, wt.data(), wt.size())) {
        LOG(debug("Marking client {} for removal due to out-of-band send error.", id));
        c.remove = true;
    }
}

server_rp server::serve(int port)
{
    server_rp res { server_code::OK, "" };

    std::unique_lock<std::mutex> lock(mutex);

    if (running) {
        return { server_code::ERR_ALREADY_STARTED, "The server has already been started." };
    }

    if ((res = serve_init(port)).first != server_code::OK) {
        return res;
    }

    running = true;

    clients.clear();
    pollfd* pollfds;
    int pollfds_len = 1;

    pollfds = new pollfd[1];

    pollfds[0].fd = listener;
    pollfds[0].events = POLLIN;

    lock.unlock();

    int pollres;
    while (true) {
        LOG(trace("Polling..."));
        pollres = poll(pollfds, pollfds_len, -1);
        LOG(trace("Polled."));

        if (!running) {
            // Shut down.
            LOG(debug("Server shutting down in loop."));
            for (auto it = clients.begin(); it != clients.end();) {
                client_id id = (*it).first;
                it++;
                proto.complete_user(id);
                remove_client(id);
            }
            break;
        }

        if (pollres == 0) {
            // Timed out.
            continue;
        }

        // Catch polling errors.
        if (pollres == -1) {
            LOG(error("poll: {} ({})", strerror(errno), errno));
            if (errno == EINTR) {
                continue;
            } else {
                res.first = server_code::ERR_POLL;
                res.second = fmts("poll: %s (%d)", strerror(errno), errno);
                break;
            }
        }

        for (int i = 1; i < pollfds_len; i++) {
            // Such clients will be lazily removed next time
            // a client is added and pollfds and poll_ids are recreated.
            if (pollfds[i].fd == -1) {
                continue;
            }

            client_id id = poll_ids[i - 1];
            client& c = clients.at(id);

            if (pollfds[i].revents & POLLERR) {
                c.remove = true;
            }
            if (pollfds[i].revents & POLLHUP) {
                c.remove = true;
            }
            if (pollfds[i].revents & POLLIN) {
                // NB: This happens if the client close()d, too.
                // In that case recv() will return 0.
                server_rp ires = handle(id);
                if (ires.first != server_code::OK) {
                    if (ires.first != server_code::CLIENT_CLOSED) {
                        LOG(debug("handle (client {}): {} ({})", id, ires.second, static_cast<int>(ires.first)));
                    }
                    c.remove = true;
                }
            }

            if (c.remove) {
                remove_client(id);
                // "If the value if fd is less than 0, events shall be ignored..."
                pollfds[i].fd = -1;
            }
        }

        // Listener socket error.
        if (pollfds[0].revents & (POLLERR | POLLHUP)) {
            LOG(error("Error occurred on listener, shutting down ({}).", pollfds[0].revents));
            break;
        }

        if (pollfds[0].revents & POLLNVAL) {
            // Listener was close()d.
            LOG(info("Listener closed, shutting down."));
            break;
        }

        if (pollfds[0].revents & POLLIN) {
            // Listener socket can accept.
            struct sockaddr_storage client_addr;
            socklen_t addr_sz = sizeof(client_addr);

            lock.lock();

            int fd = accept(listener, reinterpret_cast<struct sockaddr *>(&client_addr), &addr_sz);

            lock.unlock();

            if (fd == -1) {
                if (errno != EINTR) {
                    LOG(error("accept: {} {}", strerror(errno), errno));
                    res.first = server_code::ERR_SOCKET_ACCEPT;
                    res.second = fmts("accept: %s (%d)", strerror(errno), errno);
                    break;
                }
            } else {
                // New client!

                // Set nonblocking.
                if (fcntl(fd, F_SETFL, O_NONBLOCK) == -1) {
                    // Guess we are not accepting this client.
                    close(fd);
                    continue;
                }

                client c;
                c.fd = fd;
                // Just something that can be free()d.
                c.buff = new uint8_t[1];
                c.i = 0;

                clients[next_client_id] = c;
                proto.add_user(next_client_id);

                // Add to the polling list.
                delete[] pollfds;
                pollfds_len = 1 + clients.size();
                pollfds = new pollfd[pollfds_len];

                pollfds[0].fd = listener;
                pollfds[0].events = POLLIN;

                poll_ids.clear();

                int i = 1;
                for (auto& kv : clients) {
                    poll_ids.push_back(kv.first);
                    pollfds[i].fd = kv.second.fd;
                    pollfds[i].events = POLLIN;
                    i++;
                }

                LOG(info("Added new client {}.", next_client_id));

                next_client_id++;
            }
        }
    }

    // cleanup

    for (auto& kv : clients) {
        close(kv.second.fd);
        delete[] kv.second.buff;
    }

    delete[] pollfds;

    lock.lock();

    close(listener);
    listener = -1;

    running = false;

    lock.unlock();

    return res;
}

void server::remove_client(client_id id)
{
    client& c = clients.at(id);

    ::shutdown(c.fd, SHUT_RDWR);
    close(c.fd);
    delete[] c.buff;
    clients.erase(id);
    proto.remove_user(id);

    LOG(info("Removed client {}.", id));
}

} // namespace fishbowl
} // namespace cuauv

#undef RET_ON_ERR
#undef LOG
#undef ATTEMPT_RECV
#undef RESET_BUFF
