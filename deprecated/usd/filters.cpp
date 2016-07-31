#include <libshm/c/shm.h>
#include <math.h>

int negate(int x) {
    return -x;
}

float oneeighty(float x){
    return fmod(x + 180, 360);
}

bool invert(bool x) {
    return !x;
}

//This is a truely hackish way to zero motors and set soft kill on hard kill
//Let's break out the side effects
bool unit_hard_kill(bool x){
    if(x){
        //We are hard killed, zero the motors; set soft kill
        shm_set(switches, soft_kill, true);
        struct motor_desires md;
        md.aft_port = 0;
        md.fore_starboard = 0;
        md.aft_starboard = 0;
        md.fore_port = 0;
        md.sway_aft = 0;
        md.sway_fore = 0;
        md.port = 0;
        md.starboard = 0;
        md.fore_port = 0;
        md.aft_starboard = 0;
        shm_setg(motor_desires, md);
    }
    return x;
}
