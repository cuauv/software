#pragma once

#include <cstdint>
#include <vector>

namespace cuauv {
namespace serial {

/**
 * @brief Interface class for physical serial links
 *
 * This interface allows fake devices to be created by implementing this class
 */
class Port {
    public:

    /// Virtual destructor
    virtual ~Port() {}

    /**
     * Gets a string name for this port
     *
     * The name returned should be a valid name to pass into the Manager constructor
     *
     * @returns a stirng representing this port
     */
    virtual std::string name() const = 0;

    /**
     * Drops any data currently held in an input or output buffer
     */
    virtual void flush() = 0;

    /**
     * Reads up to count bytes from the input stream
     *
     * @param count The number of bytes to read
     * @param timeout the timeout for the read, in milliseconds. If set to 0, the read will never time out
     *
     * @return up to count bytes of data read. If a timeout occurs, the function returns as many bytes
     * as were read before the timeout
     */
    virtual std::vector<uint8_t> read(unsigned int count, unsigned int timeout) = 0;

    /**
     * Writes data to the output stream. Blocks until the data has been fully written
     */
    virtual void write(std::vector<uint8_t> data) = 0;
};

}} // end namespace cuauv::serial
