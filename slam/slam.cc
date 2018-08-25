#include "slam_server.h"
#include "slam_filter.h"
#include "conf.h"
#include <thread>
#include <libshm/c/shm.h>
#include <unistd.h>

using namespace std::chrono;

struct kalman k;
struct navigation_desires nd;
struct switches sw;

int main(int argc, char** argv) {

    shm_init();

    SlamFilter filter(NUM_PARTICLES);
    SlamServer server(&filter);
    std::thread server_thread([&] (SlamServer *server) { server->Listen(); }, &server);

    while (true) {
        unsigned start = duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
#ifdef VERBOSE
        std::cout << "ITER!" << std::endl;
#endif

        shm_getg(kalman, k);
        shm_getg(navigation_desires, nd);
        shm_getg(switches, sw);
        vec6 update;

	if (sw.soft_kill) {
	    update << 0,0,0,0,0,0;
	}
	else {
            float ns = cos(k.heading*3.14/180)*nd.speed - sin(k.heading*3.14/180)*nd.sway_speed;
            float es = sin(k.heading*3.14/180)*nd.speed + cos(k.heading*3.14/180)*nd.sway_speed;
            update <<  ns, es, k.depth, k.heading, k.pitch, k.roll;
	}
        filter.Update(update);
#ifdef PLOT
        filter.GNUPlotOut();
#endif
#ifdef VERBOSE
        std::cout << filter << std::endl;
#endif

        unsigned end = duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
        unsigned delta = DT*1000 - (end - start);
        std::this_thread::sleep_for(milliseconds(delta));

    }

    return 0;
}



