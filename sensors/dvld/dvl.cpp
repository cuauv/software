#include "dvl.h"
#include "pd5Com.h"
#include <math.h>
#include <cuauv/cpp/shmsharedvar.h>

#define DEPTH_OFFSET 0.7
#define TRANS_ANGLE 30 //22.5
#define ROTATION -45
#define deg_2_rad(a) (3.1415926*a/180)

using namespace AUV;
using namespace sensord;

void dvl::start(const string& port, const long uInterval) {
    Pd5Com comm (port);
    dvl_pd5 packet;
    
    //setup the shared variables
    ShmSharedVar<double> altitude1("/sensors/dvl/altitudes/1");
    ShmSharedVar<double> altitude2("/sensors/dvl/altitudes/2");
    ShmSharedVar<double> altitude3("/sensors/dvl/altitudes/3");
    ShmSharedVar<double> altitude4("/sensors/dvl/altitudes/4");

    //"smart" average
    ShmSharedVar<double> savg_alt("/sensors/dvl/savg_altitude");

    ShmSharedVar<double> heading("/sensors/dvl/heading");
    ShmSharedVar<double> pitch("/sensors/dvl/pitch");
    ShmSharedVar<double> roll("/sensors/dvl/roll");

    ShmSharedVar<double> temperature("/sensors/dvl/temperature");
    ShmSharedVar<double> depth ("/sensors/dvl/depth");
    
    ShmSharedVar<double> velocity_x("/sensors/dvl/velocity/x");
    ShmSharedVar<double> velocity_y("/sensors/dvl/velocity/y");
    ShmSharedVar<double> velocity_z("/sensors/dvl/velocity/z");

    ShmSharedVar<double> position_x("/sensors/dvl/dmg/x"); 
    ShmSharedVar<double> position_y("/sensors/dvl/dmg/y"); 
    ShmSharedVar<double> position_z("/sensors/dvl/dmg/z"); 
    
    ShmSharedVar<bool> low_amp1("/sensors/dvl/low_amp/1");
    ShmSharedVar<bool> low_cor1("/sensors/dvl/low_correlation/1");
    ShmSharedVar<bool> low_amp2("/sensors/dvl/low_amp/2");
    ShmSharedVar<bool> low_cor2("/sensors/dvl/low_correlation/2");
    ShmSharedVar<bool> low_amp3("/sensors/dvl/low_amp/3");
    ShmSharedVar<bool> low_cor3("/sensors/dvl/low_correlation/3");
    ShmSharedVar<bool> low_amp4("/sensors/dvl/low_amp/4");
    ShmSharedVar<bool> low_cor4("/sensors/dvl/low_correlation/4");
    
    double del_x;
    double prev_x = 0;
    double del_y;
    double prev_y = 0;

    double temp_pitch = 0;
    double temp_roll = 0;

    for(;;) {
        //read a new packet from DVL
        packet = comm.getData();
        
        if (packet.DVL_DATA_IDh != 0) {
        
            //write to shared memory
            altitude1 = (double)((packet.RNG_TO_BTM)[0])/100.0;
            altitude2 = (double)((packet.RNG_TO_BTM)[1])/100.0;
            altitude3 = (double)((packet.RNG_TO_BTM)[2])/100.0;
            altitude4 = (double)((packet.RNG_TO_BTM)[3])/100.0;

            //Smart average of altitude
            //This compensates for the angle of the beams - they do not go straight down
            savg_alt = cos(deg_2_rad(TRANS_ANGLE))*((altitude1.value() + altitude2.value()
                        + altitude3.value() + altitude4.value()) / 4.0);

            heading = (double)((packet.HEADING)/100.0);
            temp_pitch = (double)((packet.PITCH)/100.0);
            temp_roll = (double)((packet.ROLL)/100.0);
            
            pitch = temp_pitch * cos(deg_2_rad(ROTATION)) + temp_roll*sin(deg_2_rad(ROTATION));
            roll = temp_roll * cos(deg_2_rad(ROTATION)) - temp_pitch*sin(deg_2_rad(ROTATION));

            depth = ((double)(packet.DEPTH) / 10.0) - DEPTH_OFFSET;
            temperature = (double)(packet.TEMPERATURE) / 100.0;

            double velocity_x_tmp, velocity_y_tmp, velocity_z_tmp;
            velocity_x_tmp = (double)(packet.BTM_VEL.y)/1000.0;
            velocity_y_tmp = (double)(packet.BTM_VEL.x)/1000.0;
            velocity_z_tmp = (double)(packet.BTM_VEL.z)/1000.0;

            // ignore ridiculous values that make peter cry
            if (fabs(velocity_x_tmp) < 5) {
                velocity_x = velocity_x_tmp;
            }
            if (fabs(velocity_y_tmp) < 5) {
                velocity_y = velocity_y_tmp;
            }
            if (fabs(velocity_z_tmp) < 5) {
                velocity_z = velocity_z_tmp;
            }

            position_x = (double)((packet.DIST_MADE_GOOD_BTM.y)/1000.0);
            position_y = (double)((packet.DIST_MADE_GOOD_BTM.x)/1000.0);

            //TODO: Figure out why this is 10.0 instead of 1000.0. 
            ///     1000.0 really seems to be the right value, but 10.0 was in use last
            //position_z = (double)(packet.DIST_MADE_GOOD_BTM.z)/1000.0;
            position_z = (double)(packet.DIST_MADE_GOOD_BTM.z)/10.0;

            //Integration of local (x,y) position to get world (north, east) position
            del_x = *position_x - prev_x;
            del_y = *position_y - prev_y;
        
            prev_x = *position_x;
            prev_y = *position_y;

            //recover bottom status
            low_amp1 = packet.BOTTOM_STATUS & 0x01;
            low_cor1 = packet.BOTTOM_STATUS & 0x02;
            low_amp2 = packet.BOTTOM_STATUS & 0x04;
            low_cor2 = packet.BOTTOM_STATUS & 0x08;
            low_amp3 = packet.BOTTOM_STATUS & 0x10;
            low_cor3 = packet.BOTTOM_STATUS & 0x20;
            low_amp4 = packet.BOTTOM_STATUS & 0x40;
            low_cor4  = packet.BOTTOM_STATUS & 0x80;
        }

        //sleep:
        usleep(uInterval);
    }
}
