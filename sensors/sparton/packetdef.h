#include <stdint.h>

#ifndef PACKDEF_H
#define PACKDEF_H


struct __attribute__ ((__packed__)) rfsMessage
{
    uint8_t Revision;
    uint32_t Payload;

    uint8_t Command;
    // Command= 0:Get_Response 9:Value_Is 127:Error 12:Construct
    uint8_t Sequence;
    uint8_t VID;
};

struct __attribute__ ((__packed__)) scalarPayload
{
    // Data description
    uint8_t SAP; //For scalars, expect 0x1X
    uint8_t type; //3 For float32, 4 for float64
    uint8_t field_size; //Size of scalar in bytes

    // Data
    float scalar;
};

struct __attribute__ ((__packed__)) scalarIntPayload
{
    // Data description
    uint8_t SAP; //For scalars, expect 0x1X
    uint8_t type; //0 For int
    uint8_t field_size; //Size of scalar in bytes

    // Data
    uint32_t scalar;
};

struct __attribute__ ((__packed__)) rfsAndIntPayload
{
    //rfsMessage
    rfsMessage message;
    //IntPayload
    scalarIntPayload payload;
};

// Payload for the values with three scalars
// for example, accel and gyro outputs
struct __attribute__ ((__packed__)) triplePayload
{
    // Data description
    uint8_t SAP; //For arrays, expect 0x2X
    uint8_t type;
    uint8_t fieldsize;
    uint8_t start_index;
    uint8_t stop_index;

    // Data
    float A;
    float B;
    float C;
};

struct __attribute__ ((__packed__)) positionPayload
{

    // Data description
    uint8_t SAP;
    uint8_t type;
    uint8_t fieldsize;
    /*/
    uint8_t row_start;
    uint8_t col_start;
    uint8_t row_end;
    uint8_t col_end;
    */

    // Data
    float pitch;
    float roll;
    float yawt;

    float magErr;
    float temperature;

    float magpx;
    float magpy;
    float magpz;

    float accelpx;
    float accelpy;
    float accelpz;

    float gyropx;
    float gyropy;
    float gyropz;
};

#endif
