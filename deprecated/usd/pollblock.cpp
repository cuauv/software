#include <limits.h>
#include <iostream>

#include "pollblock.h"

/*!
 * \brief Creates a PollBlock.
 *
 * \param device Device owning this PollBlock.
 * \param pollInterval Poll interval of this PollBlock in microseconds.
 * \param address Start address of this PollBlock.
 * \param length Length of this PollBlock in registers.
 */
PollBlock::PollBlock(long pollInterval, uint16_t address, uint8_t length)
    : startAddress(address), length(length), pollInterval(pollInterval) {
    // If auto-polling, set stuff up.
    if (pollInterval > 0) {
        pollIntervalTime.tv_usec = pollInterval % 1000000;
        pollIntervalTime.tv_sec = pollInterval / 1000000;

        // Initial poll from device on startup.
        gettimeofday(&nextPoll, NULL);
    }
    else {
        // The device should not add this block to the poll queue...
        // But what the heck, ensure we never poll anyway.
        nextPoll.tv_sec = LONG_MAX;
    }
}

/*!
 * \brief Polls the PollBlock.
 *
 * Called by the main/control thread of the Device. Will call makeReadRequest on
 * the Device. Will calculate the next poll time in accordance with this
 * PollBlock's auto-poll frequency, and notify all variables in this PollBlock
 * that they have been polled.
 */
void PollBlock::poll() {
    timeval now;
    gettimeofday(&now, NULL);

    // Update the next poll time if this block auto-polls.
    if (pollInterval > 0) {
        timeradd(&now, &pollIntervalTime, &nextPoll);
    }
}

/*!
 * \brief Returns the start address of this PollBlock.
 *
 * \return the start address of this PollBlock.
 */
uint16_t PollBlock::getStartAddress() const {
    return startAddress;
}

/*!
 * \brief Returns the length of this PollBlock.
 *
 * \return the length of this PollBlock.
 */
uint8_t PollBlock::getLength() const {
    return length;
}

/*!
 * \brief Returns the next poll time of this PollBlock.
 *
 * \return the next poll time of this PollBlock.
 */
timeval PollBlock::getNextPollTime() const {
    return nextPoll;
}

/*!
 * \brief Returns true if this PollBlock auto-polls.
 *
 * A PollBlock auto-polls if its poll interval is nonzero.
 * If the poll interval is zero, this PollBlock probably contains write-only
 * variables, variables that the device pushes when updated, or variables that
 * the computer only polls if it feels like it, for whatever reason. (Except
 * there's no support for on-demand polling... yet...)
 *
 * \return true if this Pollblock auto-polls.
 */
bool PollBlock::isAutoPoll() const {
    return pollInterval > 0;
}
