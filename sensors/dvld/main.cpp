#include <cstdio>
#include "dvld.h"
#include "libshm/c/shm.h"

using namespace std;
using namespace sensord;

int enter_dvld(char* dev);

	int main(int argc, char** argv)
	{
		if (argc < 2) {
			printf("Usage: auv-dvld [device]\n");
			return 1;
		}
		
		printf("dvld running...\n");

        shm_init();

		try {
			enter_dvld(argv[1]);
		} catch (const exception& err) {
			printf("Exception caught: %s\n", err.what());
			return 1;
		}

		return 0;
	}

int enter_dvld(char* dev) {
    dvld::start(dev, 0);
    return 0;
}

//@ thor.sensors.dvl.ticking = shm.dvl.tick.get() != delayed(1.0, 'shm.dvl.tick.get()')
