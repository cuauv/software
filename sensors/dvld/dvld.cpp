#include "dvld.h"
#include "pd5Com.h"
#include <math.h>

#include "libshm/c/shm.h"

#include "pd5Com.h"

#define DEPTH_OFFSET 0.0 //TODO: determine this in water
#define TRANS_ANGLE 30 //22.5
#define deg_2_rad(a) (3.1415926*a/180)

using namespace sensord;

using namespace dvl;

double get_heading() {
    double heading;
    shm_get(kalman, heading, heading);
    return heading;
}

void dvld::start(const string& port, const long uInterval) {
    Pd5Com comm (port);
    dvl_pd5 packet;

    double velocity_x_tmp, velocity_y_tmp, velocity_z_tmp;

    double del_x;
    double prev_x = 0;
    double del_y;
    double prev_y = 0;

    //reset position
    struct dvl svs; // Means SharedVars
    shm_getg(dvl, svs);
    svs.dmg_north = 0;
    svs.dmg_east = 0;

    for(;;) {
        usleep(uInterval);
        shm_getg(dvl, svs);
        //read a new packet from DVL
        packet = comm.getData();
        
        if (packet.DVL_DATA_IDh != 0) {
        
            svs.altitude_1 = (double)((packet.RNG_TO_BTM)[0])/100.0;
            svs.altitude_2 = (double)((packet.RNG_TO_BTM)[1])/100.0;
            svs.altitude_3 = (double)((packet.RNG_TO_BTM)[2])/100.0;
            svs.altitude_4 = (double)((packet.RNG_TO_BTM)[3])/100.0;

            svs.heading = (double)(packet.HEADING)/100.0;
            svs.pitch = (double)((packet.PITCH)/100.0);
            svs.roll = (double)((packet.ROLL)/100.0);

            svs.depth = ((double)(packet.DEPTH) / 10.0) - DEPTH_OFFSET;
            svs.temperature = (double)(packet.TEMPERATURE) / 100.0;

            velocity_x_tmp = (double)(packet.BTM_VEL.x)/1000.0;
            velocity_y_tmp = (double)(packet.BTM_VEL.y)/1000.0;
            velocity_z_tmp = (double)(packet.BTM_VEL.z)/1000.0;

            // ignore ridiculous values that make peter cry
            if (fabs(velocity_x_tmp) < 5) {
                svs.velocity_x = velocity_x_tmp;
            }
            if (fabs(velocity_y_tmp) < 5) {
                svs.velocity_y = velocity_y_tmp;
            }
            if (fabs(velocity_z_tmp) < 5) {
                svs.velocity_z = velocity_z_tmp;
            }

            svs.dmg_x = (double)((packet.DIST_MADE_GOOD_BTM.y)/1000.0);
            svs.dmg_y = (double)((packet.DIST_MADE_GOOD_BTM.x)/1000.0);
            svs.dmg_z = (double)(packet.DIST_MADE_GOOD_BTM.z)/1000.0;

            del_x = svs.dmg_x - prev_x;
            del_y = svs.dmg_y - prev_y;
        
            // NOTE: this output is deprecated
            // Kalman north/east should be used instead of dvl.dmg_north/east
            double hdg_rad = deg_2_rad(get_heading());
            svs.dmg_north += (del_x * cos(hdg_rad) - del_y * sin(hdg_rad));
            svs.dmg_east += (del_y * cos(hdg_rad) + del_x * sin(hdg_rad));
        
            prev_x = svs.dmg_x;
            prev_y = svs.dmg_y;
            svs.dmg_z = (double)(packet.DIST_MADE_GOOD_BTM.z)/10.0;

            //recover bottom status
            svs.low_amp_1 = (packet.BOTTOM_STATUS & 0x01) != 0;
            svs.low_correlation_1 = (packet.BOTTOM_STATUS & 0x02) != 0;
            svs.low_amp_2 = (packet.BOTTOM_STATUS & 0x04) != 0;
            svs.low_correlation_2 = (packet.BOTTOM_STATUS & 0x08) != 0;
            svs.low_amp_3 = (packet.BOTTOM_STATUS & 0x10) != 0;
            svs.low_correlation_3 = (packet.BOTTOM_STATUS & 0x20) != 0;
            svs.low_amp_4 = (packet.BOTTOM_STATUS & 0x40) != 0;
            svs.low_correlation_4  = (packet.BOTTOM_STATUS & 0x80) != 0;


#define CHECK_BEAM(n) \
  if (!svs.low_amp_##n && !svs.low_correlation_##n) {\
    good++;\
    alt_sum += svs.altitude_##n;\
  }

            int good = 0;
            double alt_sum = 0.0;
            CHECK_BEAM(1)
            CHECK_BEAM(2)
            CHECK_BEAM(3)
            CHECK_BEAM(4)

            double alt_avg = 0.0;
            if (good) {
              alt_avg = alt_sum / good;
            }

            svs.savg_altitude = cos(deg_2_rad(TRANS_ANGLE)) * alt_avg;

            // Indicate that we are receiving data
            svs.tick++;

            //write to shared memory
            shm_setg(dvl, svs);
        }
    }
}
