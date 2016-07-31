#ifndef PD5COM_H
#define PD5COM_H

#include "dvlSerial.h"

#define TIMEOUT 2000000
#define SETUP_PAUSE 100000  

#include <string>

using namespace std;
using namespace sensord;

namespace sensord {
namespace dvl {
    
//A crude struct for a vector3 with error
template <class T>
struct vect3_w_err {
    T x;
    T y;
    T z;
    T err;
};

//the raw data output by the DVL
typedef struct dvl_pd5 {
    u_int8_t DVL_DATA_IDh;

    u_int8_t DATA_STRUCTURE;
    u_int16_t NO_OF_BYTES;
    u_int8_t SYSTEM_CONFIG;

    struct vect3_w_err <int16_t> BTM_VEL;
    
    u_int16_t RNG_TO_BTM[4];
    
    u_int8_t BOTTOM_STATUS;
    
    struct vect3_w_err <int16_t> REF_VEL;

    int16_t REF_LAYER_START;
    int16_t REF_LAYER_END;
    
    u_int16_t REF_LAYER_STATUS;

    u_int8_t TOFP_HOUR;
    u_int8_t TOFP_MINUTE;
    u_int8_t TOFP_SECONDS;
    u_int8_t TOFP_HUNDREDTHS;
    u_int16_t BIT_RESULTS;

    u_int16_t SPEED_OF_SOUND;
    u_int16_t TEMPERATURE;
    u_int8_t SALINITY;
    
    u_int16_t DEPTH;
    
    int16_t PITCH; 
    int16_t ROLL;
    u_int16_t HEADING;

    //on these next two:
    // x = east
    // y = north
    // z = up
    // err = err
    struct vect3_w_err <int32_t> DIST_MADE_GOOD_BTM;
    struct vect3_w_err <int32_t> DIST_MADE_GOOD_REF;

    u_int16_t CHECKSUM;
} dvl_pd5;


//a cute class to get the raw data
//uses the "PD5" data format
class Pd5Com {
    public:
        Pd5Com(const string& port);
        void setupDVLSerialPort();
        dvl_pd5 getData();

    protected:
        u_int8_t readByte();
        u_int16_t readUnsignedShort();
        int16_t readShort();
        int32_t readInt();
        u_int16_t readChecksum();

        //data members:
        DVLSerialPort ser;
        u_int16_t checksum;

        //a temporary value; used in every function, so pulled up for speed
        u_int8_t temp;
};

};

};

#endif
