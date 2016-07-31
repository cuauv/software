/*!
 * \file pollblock.h
 * \author Peter Tseng
 */

#ifndef _USD_POLLBLOCK_H_
#define _USD_POLLBLOCK_H_

#include <stdint.h>
#include <sys/time.h>

/*!
 * \class PollBlock
 * \brief A block of Synced Variables to be polled at once.
 *
 * A PollBlock is formed when any variables in contiguous registers have equal
 * poll intervals.
 * Two variables are contiguous if the address of the lower-addressed register
 * plus the length of the variable in the lower-addressed register is equal to
 * the address of the higher-addressed register.
 */
class PollBlock {
public:
    PollBlock(long pollInterval, uint16_t address, uint8_t length);

    void poll();

    uint16_t getStartAddress() const;
    uint8_t getLength() const;
    timeval getNextPollTime() const;
    bool isAutoPoll() const;

private:
    //! Start address of this PollBlock.
    uint16_t startAddress;
    //! Total length in registers of this PollBlock.
    uint8_t length;

    //! Poll interval of this PollBlock in microseconds.
    long pollInterval;
    //! Timeval representing the poll interval of this PollBlock.
    timeval pollIntervalTime;
    //! Timeval holding the next poll time of this Pollblock.
    timeval nextPoll;
};

#endif
