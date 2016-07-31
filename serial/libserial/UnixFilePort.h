#pragma once

#include <string>

#include "Port.h"

namespace cuauv {
namespace serial {

/**
 * @brief A Port which transmits and receives using unix file APIs
 */
class UnixFilePort : public Port {
    public:
        /**
         * Creates a UnixFilePort based on two files.
         *
         * @param txFile the file to transmit on
         * @param rxFile the file to receive on
         */
        UnixFilePort(const std::string& txFile, const std::string& rxFile, const std::string& displayName);

        /**
         * Destroys a UnixFilePort, closing any file descriptors it has open
         */
        ~UnixFilePort();

        virtual std::string name() const override;

        virtual void flush() override;

        virtual std::vector<uint8_t> read(unsigned int count, unsigned int timeout) override;

        virtual void write(std::vector<uint8_t> data) override;

    protected:
        /**
         * Creates a UnixFilePort from one already prepared file descriptor.
         * The same file descriptor is used for both transmit and receive
         *
         * @param fd the fd to transmit and receive on
         */
        UnixFilePort(int fd, const std::string& displayName);

        /// The file descriptor to transmit on
        int m_fd_tx;
        /// The file descriptor to receive on
        int m_fd_rx;

        /// Extra fd which will allow open() to succeed on a FIFO without another
        //  process being connected
        int m_fd_tx_read;

        /// A friendly name for this port
        const std::string m_name;
};


}} // end namespace cuauv::serial
