#include "pd5Com.h"
#include "../serial/serial.h"

using namespace sensord;
using namespace dvl;

// Whether we are the WORKHORSE versus the EXPLORER
// Explorer is the new one
//#define WORKHORSE

Pd5Com::Pd5Com (const string &port): ser(port.c_str()) {
    setupDVLSerialPort();
}

//TODO: comment this
void Pd5Com::setupDVLSerialPort() {
	printf("Initalizing DVL!\n");
    ser.sendBreak(300);
    sleep(1);
    ser.writeSer((const unsigned char*)"CR0\n",4); //Use user set parameters
    usleep(SETUP_PAUSE);
    ser.writeSer((const unsigned char*)"PD5\n",4); //Format of DVL data packets
    usleep(SETUP_PAUSE);
   	ser.writeSer((const unsigned char*)"TP00:00.00\n",11); //Time between pings (set to 0 to maximize ping rate)
    usleep(SETUP_PAUSE);
   	ser.writeSer((const unsigned char*)"TE00:00:00.00\n",14); //Time between pings (set to 0 to maximize ping rate)
    usleep(SETUP_PAUSE);
  	ser.writeSer((const unsigned char*)"BT00001\n",8); //One ping per ensemble
    usleep(SETUP_PAUSE);
   	ser.writeSer((const unsigned char*)"CF11110\n",8); //Flow control (auto everything, but disable data recording)
    usleep(SETUP_PAUSE);
  	ser.writeSer((const unsigned char*)"CB411\n",6); //Baud Rate of 9600
    usleep(SETUP_PAUSE);
#ifdef WORKHORSE
  	ser.writeSer((const unsigned char*)"EA-04500\n",9); //Heading settage
#else
  	ser.writeSer((const unsigned char*)"EA+04500\n",9); //Heading settage
#endif
    usleep(SETUP_PAUSE);
    // TODO: EZ is marked as new command for the Explorer and has a different default
    // adding extra '0' at end
    ser.writeSer((const unsigned char*)"EZ11011010\n",11); //Sensor Source
    usleep(SETUP_PAUSE);
    // TODO: this is not the default value
  	ser.writeSer((const unsigned char*)"EX10111\n",8); //Coordinate Transformation
    usleep(SETUP_PAUSE);
    // TODO:  I just added a '#' to this; command changed in explorer
    ser.writeSer((const unsigned char*)"#BS\n",4); //Clear internal position data
    usleep(SETUP_PAUSE);
    ser.writeSer((const unsigned char*)"ES00\n",5); //Salinity is zero
    usleep(SETUP_PAUSE);
    // Sets us to ignore pings less than 60 cm away
    // Should help with DVL beam blockage
    //ser.writeSer((const unsigned char*)"#BB0060\n",8);
    //usleep(SETUP_PAUSE);
  	ser.writeSer((const unsigned char*)"CS\n",3); //Start pinging
    ser.flushInput();
    printf("Done initalizing dvl!\n");
}

u_int8_t Pd5Com::readByte() {
    u_int8_t ret;
    if(ser.readnWithTimeout(&ret, 1, TIMEOUT) != 1) {
	    printf("read error in byte - no data received in last %d microseconds\n",
             TIMEOUT);
	    return 0;
    }
    checksum += ret;
    return ret;
}

u_int16_t Pd5Com::readUnsignedShort() {
    u_int16_t ret = 0;
    
    for (u_int8_t i = 0; i < 2; i++) {
        if(ser.readnWithTimeout(&temp, 1, TIMEOUT) != 1) {
		printf("read error in unsigned short\n");
		return 0;
	}
        checksum+= temp;
        ret += temp << (8*i);
    }
    
    return ret;

}

int16_t Pd5Com::readShort() {
    return (int16_t)readUnsignedShort();
}

int32_t Pd5Com::readInt() {
    int32_t ret = 0;

    for (u_int8_t i = 0; i < 4; i++) {
        if(ser.readnWithTimeout(&temp, 1, TIMEOUT) != 1) {
		printf("error in readInt\n");
		return 0;
	}
        checksum+= temp;
        ret += temp << (8*i);
    }
    return ret;
    
}

u_int16_t Pd5Com::readChecksum() {
    u_int16_t ret = 0;

    for (u_int8_t i = 0; i < 2; i++) {
        if (ser.readnWithTimeout(&temp, 1, TIMEOUT) != 1) {
            printf("Error reading checksum\n");
            return 0;
        }
        ret += temp << (8*i);
    }

    return ret;
}

dvl_pd5 Pd5Com::getData() {
    checksum = 0;

    dvl_pd5 ret;
    u_int8_t t = readByte();

    if (t == 0x7d) {
        ret.DVL_DATA_IDh = t;
	    
        ret.DATA_STRUCTURE = readByte();

        ret.NO_OF_BYTES = readUnsignedShort();

        ret.SYSTEM_CONFIG = readByte();

        ret.BTM_VEL.x = readShort();
        ret.BTM_VEL.y = readShort();
        ret.BTM_VEL.z = readShort();
        ret.BTM_VEL.err = readShort();

        (ret.RNG_TO_BTM)[0] = readUnsignedShort();
        (ret.RNG_TO_BTM)[1] = readUnsignedShort();
        (ret.RNG_TO_BTM)[2] = readUnsignedShort();
        (ret.RNG_TO_BTM)[3] = readUnsignedShort();

        ret.BOTTOM_STATUS = readByte();

        ret.REF_VEL.x = readShort();
        ret.REF_VEL.y = readShort();
        ret.REF_VEL.z = readShort();
        ret.REF_VEL.err = readShort();

        ret.REF_LAYER_START = readShort();
        ret.REF_LAYER_END = readShort();

        ret.REF_LAYER_STATUS = readByte();

        ret.TOFP_HOUR = readByte();
        ret.TOFP_MINUTE = readByte();
        ret.TOFP_SECONDS = readByte();
        ret.TOFP_HUNDREDTHS = readByte();
        ret.BIT_RESULTS = readUnsignedShort();
        
        ret.SPEED_OF_SOUND = readUnsignedShort();
        ret.TEMPERATURE = readUnsignedShort();
        ret.SALINITY = readByte();

        ret.DEPTH = readUnsignedShort();

        ret.PITCH = readShort();
        ret.ROLL = readShort();
        ret.HEADING = readShort();

        ret.DIST_MADE_GOOD_BTM.x = readInt();
        ret.DIST_MADE_GOOD_BTM.y = readInt();
        ret.DIST_MADE_GOOD_BTM.z = readInt();
        ret.DIST_MADE_GOOD_BTM.err = readInt();

        ret.DIST_MADE_GOOD_REF.x = readInt();
        ret.DIST_MADE_GOOD_REF.y = readInt();
        ret.DIST_MADE_GOOD_REF.z = readInt();
        ret.DIST_MADE_GOOD_REF.err = readInt();

        ret.CHECKSUM = readChecksum();

        if (checksum == ret.CHECKSUM) {
            //printf("Checksum meet!\n");
            return ret;
        } else {
            printf("DVL Checksum failed\n");
        }
    } else {
        //printf("DVL: Failure with this \"t\" thing: t=%x\n", t);
        //sleep(1); //so we don't overflow the log
	    //maybe this isn't a problem? IT IS A PROBLEM
    }

    ser.flushBuffers();
    
    ret.DVL_DATA_IDh = 0;
    return ret;
} 
