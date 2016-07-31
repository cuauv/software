#include "protocol.h"

/*!
 * \brief Computes the simple add-everything-together checksum.
 *
 * \param size Size of the message
 * \param message Message to compute a checksum for.
 * \return Checksum for the given message.
 */
uint8_t computeChecksum(int size, uint8_t* message) {
    uint8_t checksum = 0;
    for (int i = 0; i < size; ++i) {
        checksum = (checksum + message[i]) % 256;
    }
    return checksum;
}
