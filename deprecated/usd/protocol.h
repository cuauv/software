/*!
 * \file protocol.h
 * \author Peter Tseng
 */

#ifndef _USD_PROTOCOL_H
#define _USD_PROTOCOL_H

#include <stdint.h>

/*!
 * \brief The max packet length under the protocol: 255 registers
 * (2 bytes each), function (1), count (1), address (2), and checksum (1).
 */
static const int MAX_PACKET_LENGTH = 515;
//! The initial part of a packet: function (1), count (1), address (2).
static const int BASE_PACKET_LENGTH = 4;

//! Length of ID in registers.
static const uint8_t ID_LENGTH = 1;
//! ID register address.
static const uint16_t ID_ADDRESS = 0;


//! Enum of all recognized packet types.
enum {
    PACKET_TYPE_NOP = 0x00,
    PACKET_TYPE_READ = 0x01,
    PACKET_TYPE_WRITE = 0x02,
    PACKET_TYPE_OR = 0x03,
    PACKET_TYPE_AND = 0x04,
    PACKET_TYPE_XOR = 0x05,

    PACKET_TYPE_RESPONSE = 0x40,

    PACKET_TYPE_INVALID_REGISTER = 0x81,
    PACKET_TYPE_READONLY_REGISTER = 0x82,
    PACKET_TYPE_BAD_LENGTH = 0x83,
    PACKET_TYPE_BAD_CHECKSUM = 0x84,
    PACKET_TYPE_INVALID_FUNCTION = 0x85,
    PACKET_TYPE_UNKNOWN_FAILURE = 0xBF,

    PACKET_TYPE_ERROR = 0x80,

    PACKET_TYPE_RESTART = 0xAA
};

uint8_t computeChecksum(int size, uint8_t* message);

#endif
