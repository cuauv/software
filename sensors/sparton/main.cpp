/* Daemon for the Sparton Compass
 * 
 * Outputs in degrees (for orientation), g's (for acceleration)
 * and in degrees/second (for angular rates)
 * Compass updates at 100Hz.
 *
 * Coordinate system:
 * Hold the sparton flat with the "sparton" text up-right to you.
 * +X axis is going forward (up for the text)
 * +Y axis is going right (along the text)
 * +Z axis is going DOWN (not up!)
 *
 * +Heading is around +Z axis (right-hand-rule)
 * +Roll is around +X
 * +Pitch is around +Y
 */

#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <stdio.h>
#include "packetdef.h"
#include "../serial/serial.h"
#include <netinet/in.h>
#include <exception>
#include <signal.h>
#include <algorithm>
#include "libshm/c/shm.h"

#define BAUD_RATE           115200
#define REVISION            3
#define ERROR_AND_PROTO     0x00
#define DEFAULT_USE_RATE    false

//Delays and timeouts
#define HEADER_CYCLE_LIMIT  10
#define MAX_PACKET_LENGTH   80
#define BAD_PACKET_DELAY    2500
#define SERIAL_TIMEOUT      10000
#define MAX_REFRESH_RATE    50 //Hz

// Output
#define OUTPUT_INTERVAL     30 // In seconds, must be >= 2.
#define OUTPUT_ON           1

//SAPP Reserved Sequences
#define SOH                 0x01
#define ETX                 0x03
#define SYN                 0x16
#define DLE                 0x10
#define ACK                 0x06
#define NAK                 0x15

//Used for DLE escape sequences
#define MASK_UP             0x80
#define MASK_OFF            0x7F

//Variable IDs
#define VID_PITCH           8
#define VID_ROLL            9
#define VID_YAW             10
#define VID_ACCEL           23
#define VID_GYRO            25
#define VID_MAG_ERROR       36
#define VID_TEMPERATURE     88
#define VID_POSITION        28
#define VID_POSITION_RATE   27

//Command numbers
#define CMD_GET_VALUE       8
#define CMD_VALUE_IS        9
#define CMD_SET             3       

using namespace sensorserial;

bool useRate = DEFAULT_USE_RATE;

uint8_t sequenceNumber = 0;
bool calibrate = false;
bool packetStatus = true;
bool sequenceShifted = false;
int badCount = 0;
bool stopCalibrate = false;
short int computeCRC(unsigned char *buffer, unsigned char length, unsigned short int startingValue, unsigned char doLSB);

//Time variables
timeval startTimeVal;
timeval lastTimeVal;
timeval currentTimeVal;
timeval lastUpdateVal;
timeval lastOutputVal;

//Structs
struct threefloats {
    float A;
    float B;
    float C;
};
struct positionFloats {
    threefloats direction;
    float magErr;
    float temperature;
    threefloats magp;
    threefloats accelp;
    threefloats gyrop;
};


void handleSig(int sig) {
    if(calibrate && stopCalibrate == false) {
        stopCalibrate = true;
    }
    else {
        exit(0);
    }
}


uint32_t EndianSwap(uint32_t in) 
{
    uint32_t out = 0;
    out += (in & 0x000000FF) << 24;
    out += (in & 0x0000FF00) << 8;
    out += (in & 0x00FF0000) >> 8;
    out += (in & 0xFF000000) >> 24;
    return out;
}
// Takes a float as input, flips its endianness with EndianSwap, and recasts it to a float output value.
float swapAndCast(float input)
{   
    uint32_t* intInput = reinterpret_cast<uint32_t*>(&input);
    uint32_t swapped = EndianSwap(*intInput);
    float* output = reinterpret_cast<float*>(&swapped);
    return *output;
}


// Used by packetize to add DLE (0x80) escape characters to output
uint8_t isControl(uint8_t value)
{
    if(value > SYN)
    {
        return (0);
    }
    switch(value)
    {
        case SYN:
        case SOH:
        case DLE:
        case ETX:
        case ACK:
        case NAK:
        return(1);
        default : return(0);
    }
}

rfsMessage makeGetPositionMessage() {
    rfsMessage message;
    message.Revision = REVISION;
    message.Payload = 0;
    message.Command = CMD_GET_VALUE; // Get Value
    message.Sequence = sequenceNumber;
    message.VID = VID_POSITION;
    
    return message;
}

// Takes an RFS message as input and constructs an appropriate packet to transmit that message to the sparton.
int packetize(uint8_t *buf, rfsMessage message, bool valueObject, scalarIntPayload payload) {
    rfsAndIntPayload fullMessage;
    fullMessage.message = message;
    fullMessage.payload = payload;

    // Calculate CRC
    // We do this on a separate buffer, since CRC does NOT include DLE espace sequences
    uint8_t crcCalc[256] = {0};
    crcCalc[0]=ERROR_AND_PROTO;
    uint16_t crc;
        if(valueObject){
        memcpy(crcCalc+1,(uint8_t*) &fullMessage, sizeof(fullMessage));
        crc = computeCRC(crcCalc, sizeof(message)+1+sizeof(payload),0xFFFF,0);
    } else {
        memcpy(crcCalc+1,(uint8_t*) &message, sizeof(message));
        crc = computeCRC(crcCalc, sizeof(message)+1,0xFFFF,0);
    }
    
    // Start writing the actual output buffer
    uint8_t* output = buf;
    *output++ = SOH;

    //*output++ = sizeof(rfsMessage)+2; // Size of payload plus 2 CRC bytes.
    //printf("size: %d", sizeof(rfsMessage)+2);
    /// XXX This is a test length hack; it will only work for this packet XXX
    if(!valueObject){
        *output++ = 12; 
    } else {
        *output++ = 25;
    }

    *output++ = ERROR_AND_PROTO;

    // Copy in the message and apply DLEs
    int length = sizeof(message);
    if(valueObject) {
        length = sizeof(fullMessage);
    }
    uint8_t* input = (uint8_t*) &fullMessage;
    while(length--)
    {
        if(isControl(*input))
        {
            *output++ = DLE;
            *output++ = *input++ | MASK_UP;
        }
        else
        {
            *output++ = *input++;
        }
    }

    
    // Inserts DLE's in the crc values and adds them to the packet. 
    if(isControl(crc >> 8))
    {
        *output++ = DLE;
        *output++ = ((crc >> 8) | MASK_UP);
    }
    else
    {
        *output++ = (crc >> 8);
    }
    if(isControl(crc & 0xff))
    {
        *output++ = DLE;
        *output++ = ((crc & 0xff) | MASK_UP);
    }
    else
    {
        *output++ = crc & 0xff;
    }

    // Adds the closing ETX character (0x03)
    *output++ = ETX; 

    // return size of buffer written
    return output - buf; 
}



//Reads the serial port for ACK and SOH before a packet. Returns true if successful, false if it failed.
bool grabSOH(SerialPort *ser) {
    //receive the first byte of the packet
    uint8_t pstart = 0;
    //Check for acknowledgement or SOH before reading packet.
    int i = 0;
    bool ackReceived = false;
    while(i <= HEADER_CYCLE_LIMIT) {
        //Reads next byte.
        ser->readnWithTimeout((unsigned char*) &pstart, 1, SERIAL_TIMEOUT);
        if(pstart == SYN) {
            //Discard and do nothing, this is filler.
        } else if((pstart == ACK)) {
            if(!ackReceived) {
                ackReceived = true;
                i = 0;//Reset cycle count for SOH.
            } else {
                fprintf(stdout, "Received more than one ACK(%d). ",pstart);
            }
        } else if (pstart == NAK) {
            //Improper framing or checksum failed (sparton-side)
            //on the packet we just sent.
            fprintf(stdout, "\nNAK: Error with sent packet. ");
            return false;
        } else if(pstart == SOH) {
            if(!ackReceived) {
                fprintf(stdout, "SOH with no ACK ");
            }
            return true;
        }
        i++;
    }

    //Exceeded cycle limit, timing out.
    fprintf(stdout,"\nHeader timeout, no ");
    if(!ackReceived) {
        fprintf(stdout, "ACK or SOH. ");
    } else {
        fprintf(stdout, "SOH. ");
    }

    return false;
}

void setRate(int VID, bool on, SerialPort *ser) {
    rfsMessage message;
    message.Revision = REVISION;
    message.Payload = 0x05;
    message.Command = CMD_SET; //Set 
    message.Sequence = sequenceNumber;
    message.VID = VID_POSITION_RATE;

    scalarIntPayload value;
    value.SAP = 0x13;
    value.type = 0x00;
    value.field_size = 0x02;
    if(on) {
        value.scalar = 100;
    } else {
        value.scalar = 0x00;
    }

    uint8_t outPacket[256]={0};
    
    //Creates a packet with the message to send to Sparton.
    int size = packetize(outPacket, message, true, value);
    printf("out packet: \n");
    for(int i = 0; i < 64; i ++ )
    {
        printf("%02X ", outPacket[i]);
        if( i % 16 == 15)
            printf("\n");
    }
    //Write our request packet
    ser->writeSer(outPacket, size);
    grabSOH(ser);
}
// 'ser' must be an opened serial port, 'cmd' is a 1-byte cmd to send to the 
// , 'buf' is a buffer to put the returned data (of expected length 'size') // into and must be at least 'size' in length
bool grabPacket(SerialPort *ser, uint8_t *buf) 
{
    int realLength = 0;

    /*Annoying note:
        You can't just read the length field and then process that many.
        It appears that this length is for the unDLE'd data.
        DLE actually changes this length. Therefore, the only way to
        read is to read the entire buffer byte by byte
    */

    //Read entire packet frame; eliminating DLEs
    uint8_t* bufcTemp = buf;
    bool etxReceived = false;
    if(true || (packetStatus)){
        do{
            // If timeout is too high, this line will create huge freezes
            // when an ETX is missed.
            ser->readnWithTimeout(bufcTemp, 1, SERIAL_TIMEOUT);

            //fprintf(stdout,"1\n");
            if(*bufcTemp == ETX){ //Check for end of transmission
                etxReceived = true;
            }
            else if(*bufcTemp == DLE){ //Eliminate DLE
                ser->readnWithTimeout(bufcTemp, 1, SERIAL_TIMEOUT);
                *bufcTemp = *bufcTemp & MASK_OFF;
            }
            
            //fprintf(stdout,"2\n");

            bufcTemp++;
            realLength++;
            
            //fprintf(stdout,"3\n");
            if(realLength > MAX_PACKET_LENGTH){
                fprintf(stdout, "Bad packet, length %d. did not receive ETX. Breaking. ", realLength);
                packetStatus = false;
                break;
            }
            //fprintf(stdout,"4\n");
        } while(!etxReceived);
    }

    //fprintf(stdout, "Packet Length is: %d\n", realLength);
   
    realLength -= 2; //Don't consider length byte and ETX byte in length calculation
    
    uint8_t claimedLength = (uint8_t) buf[0]; // read length from packet definition 

    if(realLength != claimedLength){ //verify packet length
        fprintf(stdout, "Packet length mismatch. Received %d but expected %d. ", realLength, claimedLength);
        packetStatus = false;
    }
    
    uint16_t crc = buf[realLength] | (buf[realLength-1] << 8);

    uint16_t crcExpect = computeCRC(buf + 1, realLength - 2,0xFFFF,0);

    if(crc != crcExpect)
    {
        fprintf(stdout, "Checksum failed: got %04X; expected %04X ", crc, crcExpect);
        packetStatus = false;
    }
    return true;
}



//Splits the packet to access the positionFloats payload.
positionFloats dePacketize(uint8_t *inPacket) {
    positionFloats out;

    //Alias the buffer as a rfsMessage, skipping the byte-count byte and error-protocol byte
    rfsMessage* inMessage = (rfsMessage*)(inPacket+2);
    rfsMessage in = *inMessage;

    // In depth debug output
    if(badCount > 10) {
        fprintf(stdout, "Rev:%02X, Pay:%02X, Com:%02X, Seq:%02X, VID:%02X\n", in.Revision, in.Payload, in.Command, in.Sequence, in.VID);
    }
    

    //Alias to the payload, starting after the rfsMessage
    positionPayload* position = (positionPayload*)(inPacket+2+sizeof(*inMessage));
    
    //printf("sequenceNumber: %d\n", sequenceNumber);
    //printf("inMessage: revision %d; payload %d; command %d; sequence %d; VID %d \n", inMessage->Revision, EndianSwap(inMessage->Payload), inMessage->Command, inMessage->Sequence, inMessage->VID);

    //Reverses the endianness of the floats in position.
    // pitch, roll, yawt
    out.direction.A = swapAndCast(position->pitch);
    out.direction.B = swapAndCast(position->roll);
    out.direction.C = swapAndCast(position->yawt);
        
    // magErr and temperature
    out.magErr = swapAndCast(position->magErr);
    out.temperature = swapAndCast(position->temperature);
    
    // Processed mag values
    out.magp.A = swapAndCast(position->magpx);
    out.magp.B = swapAndCast(position->magpy);
    out.magp.C = swapAndCast(position->magpz);
    
    // Processed accel values
    out.accelp.A = swapAndCast(position->accelpx);
    out.accelp.B = swapAndCast(position->accelpy);
    out.accelp.C = swapAndCast(position->accelpz);

    // Processed gyro values
    out.gyrop.A = swapAndCast(position->gyropx);
    out.gyrop.B = swapAndCast(position->gyropy);
    out.gyrop.C = swapAndCast(position->gyropz);

    return out;
}

bool checkPacket(uint8_t *inPacket, rfsMessage outMessage) {
    bool toReturn = true;
    rfsMessage* inMessage = (rfsMessage*)(inPacket+2);

    //Perform some sanity checks
    if(outMessage.Sequence != inMessage->Sequence) {
        printf("Sequence number mismatch: got %d expected %d. ", inMessage->Sequence, outMessage.Sequence);
        toReturn = false;
        if(inMessage->Sequence == outMessage.Sequence - 1) {
            sequenceShifted = true;
        }
    }
    if(inMessage->Command != CMD_VALUE_IS) {
        printf("Command number mismatch: got %d expected %d. ", inMessage->Command, CMD_VALUE_IS);
        toReturn = false;
    }
    if(inMessage->VID != outMessage.VID) {
        printf("Variable ID mismatch: got %d expected %d. ", inMessage->VID, outMessage.VID);
        toReturn = false;
    }
    return toReturn;
}

positionFloats readPositionValues(SerialPort *ser)
{
    uint8_t inPacket[256]={0};
    
    //Write our request packet
    positionFloats position;
    //Wait for ACK and SOH before recording the in packet.
    if(!grabSOH(ser)) {
        fprintf(stdout, "grabSOH returned false. ");
        packetStatus = false;
    } else if(badCount > 10) {
        fprintf(stdout, "grabSOH returned true. ");
    }
    //Get the body of the in packet.
    if(!grabPacket(ser, inPacket)){
        fprintf(stdout, "grabPacket returned false. ");
        packetStatus = false;
    }
    //Print out the in packet for debugging purposes.
    if(true and OUTPUT_ON) {
        printf("in packet: \n");
        for(int i = 0; i < 72; i ++ )
        {
            printf("%02X ", inPacket[i]);
            if( i % 16 == 15)
                printf("\n");
        }
    }
    position = dePacketize(inPacket);
    return position;
}
positionFloats getPositionValues(SerialPort *ser, rfsMessage outMessage)
{
    uint8_t outPacket[256]={0};
    uint8_t inPacket[256]={0};
    
    //Creates a packet with the message to send to Sparton.
    int size = packetize(outPacket, outMessage, false, scalarIntPayload());
    //Write our request packet
    ser->writeSer(outPacket, size);
    positionFloats position;
    do{
        std::fill(inPacket,inPacket+sizeof(inPacket),0);
        if(sequenceShifted) {
            sequenceShifted = false;
            fprintf(stdout, "Sparton lagging requests by one packet, reading next packet without requesting a new one.\n");
        }
        //Wait for ACK and SOH before recording the in packet.
        if(!grabSOH(ser)) {
            fprintf(stdout, "grabSOH returned false. ");
            packetStatus = false;
        } else if(badCount > 10) {
            fprintf(stdout, "grabSOH returned true. ");
        }
        //Get the body of the in packet.
        if(!grabPacket(ser, inPacket)){
            fprintf(stdout, "grabPacket returned false. ");
            packetStatus = false;
        
            //XXX: DEBUG
            /*
            printf("out packet: \n");
            for(int i = 0; i < 64; i ++ )
            {
                printf("%02X ", outPacket[i]);
                if( i % 16 == 15)
                    printf("\n");
            }
            
            if(badCount > 10){
                printf("in packet: \n");
                for(int i = 0; i < 72; i ++ )
                {
                    printf("%02X ", inPacket[i]);
                    if( i % 16 == 15)
                        printf("\n");
                }
                badCount = 0;
            }
            */
        } else if(badCount > 10) {//Packet printouts for in depth debugging.
            printf("out packet: \n");
            for(int i = 0; i < 64; i ++ )
            {
                printf("%02X ", outPacket[i]);
                if( i % 16 == 15)
                    printf("\n");
            }
            fprintf(stdout, "grabPacket returned true \n");
            printf("in packet: \n");
            for(int i = 0; i < 72; i ++ )
            {
                printf("%02X ", inPacket[i]);
                if( i % 16 == 15)
                    printf("\n");
            }
        }
        position = dePacketize(inPacket);
        if(!checkPacket(inPacket, outMessage)){
            fprintf(stdout, "checkPacket returned false. ");
            packetStatus = false;
        }
    } while(sequenceShifted);
    return position;
}

//Returns the time stored in a timeval in seconds.
//If refresh, returns the current time and stores it in the timeval.
//If microseconds, returns time in microseconds.
double getTime(timeval* timeVar, bool refresh) {
    if(refresh) {
        gettimeofday(timeVar, NULL);
    }
    return (double(timeVar->tv_sec) + (double(timeVar->tv_usec) / 1000000));
}

//A function to output a timestamp, given the last time outputted, the current time, and the number of updates since the last output.
void outputTimestamp(int recentUpdates, double currentTime, double lastTime) {
    double cycleTime = currentTime - lastTime;
    int currentSeconds = int(currentTime) % 60;
    int currentSecondsOnes = currentSeconds % 10;
    int currentSecondsTens = (currentSeconds - currentSecondsOnes) / 10;
    int currentMinutes = ((int(currentTime) - currentSeconds) / 60) % 60;
    int currentMinutesOnes = currentMinutes % 10;
    int currentMinutesTens = (currentMinutes - currentMinutesOnes) / 10;
    int currentHours = ((int(currentTime) / 60) - currentMinutes) / 60;
    fprintf(stdout, "                    Averaged %f Hz over %f seconds. Up %i:%i%i:%i%i\n", recentUpdates/(cycleTime), cycleTime, currentHours, currentMinutesTens, currentMinutesOnes, currentSecondsTens, currentSecondsOnes);
}


int main(int argc, char **argv) {
    fprintf(stdout,"Starting main\n");
    if (argc > 3 || argc < 2 || (argc == 3 && strncmp(argv[1], "-c", 2) != 0)) {
        printf("Usage: auv-spartond [-c(alibration)] SERIAL_PORT\n");
        return -1;
    }

    shm_init();

    calibrate = (strncmp(argv[1], "-c", 2) == 0);
    //TODO: Calibration code!

    signal(SIGINT, handleSig);
    
    SerialPort *serialPort;
    try {
        serialPort = new SerialPort(argv[argc-1], BAUD_RATE);
    } catch (std::exception& e) {
        fprintf(stdout, "Port %s not found\n", argv[argc-1]);
        return -1;
    }

    struct sparton sharedVars;
    shm_getg(sparton, sharedVars);

    bool toPrint = false;//Used in determining when to output a timestamp.

    //Time variables and such.
    getTime(&startTimeVal, true);
    lastTimeVal = startTimeVal;
    currentTimeVal = startTimeVal;
    lastUpdateVal = startTimeVal;
    lastOutputVal = startTimeVal;
    int updateCount = 0;//Number of updates since boot.
    int recentUpdates = 0;//Number of updates since last timestamp.
    
    if(useRate){
        setRate(VID_POSITION_RATE, true, serialPort);
    }
    
    while(1) {
        //Timer block
        double currentTime = getTime(&currentTimeVal, true) - getTime(&startTimeVal, false);//Check the current time and update it.
        double lastTime = getTime(&lastTimeVal, false) - getTime(&startTimeVal, false);
        double cycleTime = currentTime - lastTime;//Get the time since the last query.
        double minCycleTime = 1.00 / double(MAX_REFRESH_RATE);
        //If we haven't waited long enough between queries, wait a little longer.
        if((cycleTime < minCycleTime) && !useRate) {
            usleep((minCycleTime - cycleTime)*1e6);
        }
        currentTime = getTime(&currentTimeVal, true) - getTime(&startTimeVal, false);
        memcpy(&lastTimeVal, &currentTimeVal, sizeof(timeval));
        //getTime(&lastTimeVal, true);//Update the time of the last query.

        
        packetStatus = true;//Response packet is assumed to be good by default.

        //Outputs a timestamp once per output interval
        if((OUTPUT_ON)) {
            if((toPrint) && (int(currentTime) % OUTPUT_INTERVAL == 0)) {
                double lastOutputTime = getTime(&lastOutputVal, false) - getTime(&startTimeVal, false);
                outputTimestamp(recentUpdates, currentTime, lastOutputTime);
                toPrint = false;
                recentUpdates = 0;
                getTime(&lastOutputVal, true);
            } else if(int(currentTime) % OUTPUT_INTERVAL == OUTPUT_INTERVAL-1) {
                toPrint = true;
            }
        }
        positionFloats pos;
        if(useRate) {
            pos = readPositionValues(serialPort);
        } else {
            rfsMessage positionMessage = makeGetPositionMessage();//Creates a message to send to the compass for each request.
            //Gets all values using the position array.
            pos = getPositionValues(serialPort, positionMessage);
            sequenceNumber++;
        }
        /*/
        if((pos.temperature == 0) && (pos.magErr == 0)) {
            packetStatus = false;
            fprintf(stdout, "Packet contains bad data, temp=%d, mag=%d. ", pos.temperature, pos.magErr);
        }
        */
        //Flushes buffers if the packet is bad.
        if(!packetStatus) {
            usleep(BAD_PACKET_DELAY);
            if(serialPort->flushBuffers() != 0){
                fprintf(stdout, "Buffer flush failed. ");
            } else {
                fprintf(stdout, "Buffer flushed. ");
            }
            usleep(BAD_PACKET_DELAY);

            //Attempts to remake the serial port if the compass is unresponsive.
            if(badCount>5) {
                fprintf(stdout, "Compass unresponsive, trying to remake serial port. ");
                bool notDone;
                do{
                    notDone = false;
                    try {
                        delete serialPort;
                        serialPort = new SerialPort(argv[argc-1], BAUD_RATE);
                    } catch (std::exception& e) {
                        fprintf(stdout, "Port %s not found\n", argv[argc-1]);
                        notDone = true;
                    }
                } while(notDone);
            }
            fprintf(stdout,"Continuing.\n\n");
            badCount++;
            packetStatus = true;
            continue;
        }
        //Checks for alarmingly high downtimes.
        double downTime = getTime(&currentTimeVal, true) - getTime(&lastUpdateVal, false);
        if(downTime > 1.00) {
            fprintf(stdout, "Down for %f seconds.\n", downTime);
        }
        getTime(&lastUpdateVal, true);

        //Sets the shared variable group values from the 2D position array.
        sharedVars.magError = pos.magErr;
        sharedVars.temperature = pos.temperature; //Degrees celcius
        sharedVars.pitch = pos.direction.A; //In degrees
        sharedVars.roll = pos.direction.B; 
        sharedVars.heading = pos.direction.C;
        sharedVars.accelx = 9.8*pos.accelp.A / 1000.0f; //mG to G to m/s
        sharedVars.accely = 9.8*pos.accelp.B / 1000.0f;
        sharedVars.accelz = 9.8*pos.accelp.C / 1000.0f;
        sharedVars.roll_rate = pos.gyrop.A * 1000.0f; //in deg/second... but probably not??
        sharedVars.pitch_rate = pos.gyrop.B * 1000.0f;
        sharedVars.heading_rate = pos.gyrop.C * 1000.0f;

        //Sparton is oriented to right; positive pitch in sparton is negative roll, positive roll in sparton is positive pitch.
        double temp1 = sharedVars.roll;
        double temp2 = sharedVars.roll_rate;
        sharedVars.roll = -1*sharedVars.pitch;
        sharedVars.roll_rate = -1*sharedVars.pitch_rate;
        sharedVars.pitch = temp1;
        sharedVars.pitch_rate = temp2;

        shm_setg(sparton, sharedVars);
        //Records the fact that shared variables were updated. 
        if(OUTPUT_ON)
        {
            recentUpdates +=1;
            updateCount +=1;
        }
        badCount = 0;
    }
    //Clears sparton shared memory.
    sharedVars.magError = 0;
    sharedVars.temperature = 0;
    sharedVars.pitch = 0; // In degrees
    sharedVars.roll = 0;
    sharedVars.heading = 0;
    sharedVars.accelx = 0; //milli-g's to g's
    sharedVars.accely = 0;
    sharedVars.accelz = 0;
    sharedVars.roll_rate = 0;
    sharedVars.pitch_rate = 0;
    sharedVars.heading_rate = 0;
    shm_setg(sparton, sharedVars);

    delete serialPort;
}

/*/ Retired: See getPositionValues to update for use.
float getValue(SerialPort *ser, uint8_t vid)
{
    // Gives a Get Value command to the Sparton and returns the result
    // Only works on Float values.

    struct rfsMessage message;
    message.Revision = REVISION;
    message.Payload = 0;
    message.Command = CMD_GET_VALUE;
    message.Sequence = sequenceNumber;
    sequenceNumber++;
    message.VID = vid;
     
    uint8_t outPacket[256]={0};
    uint8_t inPacket[256]={0};
    int size = packetize(outPacket, message);
    grabPacket(ser, (uint8_t*) outPacket, size, inPacket);
    
    if(!packetStatus){
        return 0;
    }

    //XXX: DEBUG
    *
    printf("out packet: \n");
    for(int i = 0; i < 64; i ++ )
    {
        printf("%02X ", outPacket[i]);
        if( i % 16 == 15)
            printf("\n");
    }
    printf("in packet: \n");
    for(int i = 0; i < 64; i ++ )
    {
        printf("%02X ", inPacket[i]);
        if( i % 16 == 15)
            printf("\n");
    }
    *
    

    //Alias the buffer as a rfsMessage, skipping the byte-count byte and error-protocol byte
    struct rfsMessage* inMessage = (rfsMessage*)(inPacket+2);
    //Alias to the payload, starting after the rfsMessage
    struct scalarPayload* scalar = (scalarPayload*)(inPacket+2+sizeof(*inMessage));

    //printf("sequenceNumber: %d\n", sequenceNumber);
    //printf("inMessage: revision %d; payload %d; command %d; sequence %d; VID %d \n", inMessage->Revision, inMessage->Payload, inMessage->Command, inMessage->Sequence, inMessage->VID);

    //Perform some sanity checks
    if( !( message.Sequence == inMessage->Sequence ) )
    {
        printf("Sequence number mismatch: got %d expected %d \n", inMessage->Sequence, message.Sequence);
        packetStatus = false;
    }
    if( !(inMessage->Command == CMD_VALUE_IS) )
    {
        printf("Command number mismatch: got %d expected %d \n", inMessage->Command, CMD_VALUE_IS);
        packetStatus = false;
        return false;
    }
    if( !(inMessage->VID == message.VID) )
    {
        printf("Variable ID mismatch: got %d expected %d \n", inMessage->VID, message.VID);
    }
    if((inMessage->Sequence == 0) && (inMessage->Command == 0) && (inMessage->VID == 0))
    {
    }
    float f = swapAndCast(scalar->scalar);
    return f;
}

threefloats getThreeFloatsValue(SerialPort *ser, uint8_t vid)
{
    // Sends a Get Value command to the Sparton and processes the 
    struct rfsMessage message;
    message.Revision = REVISION;
    message.Payload = 0;
    message.Command = CMD_GET_VALUE; // Get Value
    message.Sequence = sequenceNumber;
    sequenceNumber++;
    message.VID = vid;
     
    uint8_t outPacket[256]={0};
    uint8_t inPacket[256]={0};
    int size = packetize(outPacket, message);
    grabPacket(ser, (uint8_t*) outPacket, size, inPacket);
    
    if(!packetStatus){
        threefloats empty;
        empty.A = 0;
        empty.B = 0;
        empty.C = 0;
        return empty;
    }

    //XXX: DEBUG
   *
    printf("out packet: \n");
    for(int i = 0; i < 64; i ++ )
    {
        printf("%02X ", outPacket[i]);
        if( i % 16 == 15)
            printf("\n");
    }
    printf("in packet: \n");
    for(int i = 0; i < 64; i ++ )
    {
        printf("%02X ", inPacket[i]);
        if( i % 16 == 15)
            printf("\n");
    }
    /
    

    //Alias the buffer as a rfsMessage, skipping the byte-count byte and error-protocol byte
    rfsMessage* inMessage = (rfsMessage*)(inPacket+2);
    //Alias to the payload, starting after the rfsMessage
    triplePayload* triple = (triplePayload*)(inPacket+2+sizeof(*inMessage));

    //printf("sequenceNumber: %d\n", sequenceNumber);
    //printf("inMessage: revision %d; payload %d; command %d; sequence %d; VID %d \n", inMessage->Revision, EndianSwap(inMessage->Payload), inMessage->Command, inMessage->Sequence, inMessage->VID);


    threefloats out;
    out.A = swapAndCast(triple->A);
    out.B = swapAndCast(triple->B);
    out.C = swapAndCast(triple->C);
    
    //printf("type: %d fieldsize: %d start: %d stop: %d\n", triple->fieldsize, triple->start_index, triple->stop_index);
    //printf("A: %f B: %f, C: %f \n", out.A, out.B, out.C);
    //printf("A: %08X B: %08X, C: %08X \n", triple->A, triple->B, triple->C);

    //Perform some sanity checks
    if( !( message.Sequence == inMessage->Sequence ) )
    {
        printf("Sequence number mismatch: got %d expected %d \n", inMessage->Sequence, message.Sequence);
        packetStatus = false;
    }
    if( !(inMessage->Command == CMD_VALUE_IS) )
    {
        printf("Command number mismatch: got %d expected %d \n", inMessage->Command, CMD_VALUE_IS);
    }
    if( !(inMessage->VID == message.VID) )
    {
        printf("Variable ID mismatch: got %d expected %d \n", inMessage->VID, message.VID);
    }

    return out;
}
*/

