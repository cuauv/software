#include "UnixFilePort.h"

#include <system_error>
#include <chrono>
#include <thread>

#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace cuauv {
namespace serial {

class io_error : public std::system_error {
    private:
        std::string m_msg;
    public:
    explicit io_error(const std::string& op) : std::system_error(errno, std::system_category()),
            m_msg(op + ": " + code().message())
    {
    }

    const char* what() const noexcept override {
        return m_msg.c_str();
    }
};

UnixFilePort::UnixFilePort(const std::string& txFile, const std::string& rxFile, const std::string& name) :
                    m_name(name)
{
    if ((m_fd_tx_read = open(txFile.c_str(), O_RDONLY | O_NONBLOCK)) < 0) {
        throw io_error("Failed to open TX file " + txFile);
    }
    if ((m_fd_tx = open(txFile.c_str(), O_WRONLY | O_NONBLOCK)) < 0) {
        throw io_error("Failed to open TX file " + txFile + " for real");
    }
    if ((m_fd_rx = open(rxFile.c_str(), O_RDONLY | O_NONBLOCK)) < 0) {
        throw io_error("Failed to open RX file " + rxFile);
    }

    flush();
}

UnixFilePort::UnixFilePort(int fd, const std::string& name) :
        m_fd_tx(fd),
        m_fd_rx(fd),
        m_fd_tx_read(-1),
        m_name(name)
{
    flush();
}

UnixFilePort::~UnixFilePort() {
    close(m_fd_tx);
    if (m_fd_tx != m_fd_rx) {
        close(m_fd_rx);
    }
    if (m_fd_tx_read >= 0) {
        close(m_fd_tx_read);
    }
}

std::string UnixFilePort::name() const {
    return m_name;
}

void UnixFilePort::flush() {
    // No-op, there's no such thing as flushing for simple file I/O
    return;
}

std::vector<uint8_t> UnixFilePort::read(unsigned int count, unsigned int timeout) {
    auto max_duration = std::chrono::milliseconds(timeout);
    auto end_time = std::chrono::steady_clock::now() + max_duration;

    std::vector<uint8_t> result;
    result.reserve(count);

    auto now = std::chrono::steady_clock::now();
    while (result.size() < count && now < end_time) {
        auto remaining_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - now);
        struct timeval timeout;
        timeout.tv_sec = remaining_duration.count() / 1000;
        timeout.tv_usec = (remaining_duration.count() % 1000) * 1000;

        fd_set if_set;
        FD_ZERO(&if_set);
        FD_SET(m_fd_rx, &if_set);
        if (::select(FD_SETSIZE, &if_set, NULL, NULL, &timeout) < 0) {
            throw io_error("Error while waiting for input on " + name());
        }
        
        if (FD_ISSET(m_fd_rx, &if_set)) {
            // We woke up because there's data to read. Read it!
            uint8_t buf[100];
            size_t num_to_read = std::min(count - result.size(), sizeof(buf));
            ssize_t num_read;
            if ((num_read = ::read(m_fd_rx, buf, num_to_read)) < 0) {
                if (errno == EAGAIN) {
                    // HACK: for FIFOs, if there's no device on the other side, read will
                    // just return immediately with this errno set. Explicitly sleep for
                    // the remaining time so that we don't consume a ton of CPU if there
                    // isn't a process on the other side of the FIFO
                    std::this_thread::sleep_until(end_time);
                    break;
                }
                throw io_error("Failed to read from " + name());
            }
            result.insert(result.end(), buf, buf+num_read);
        }

        now = std::chrono::steady_clock::now();
    }

    return result;
}

void UnixFilePort::write(std::vector<uint8_t> data) {
    ssize_t num_written;
    if ((num_written = ::write(m_fd_tx, data.data(), data.size())) < 0) {
        throw io_error("Failed to write to " + name());
    }
    if (((size_t)num_written) != data.size()) {
        throw io_error("Wrote fewer bytes than expected to " + name());
    }
    // Make sure all data is written out before returning
    ::fsync(m_fd_tx);
}

}} // end namespace cuauv::serial
