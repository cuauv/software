#ifndef DVLD_H
#define DVLD_H

#include <string>

using namespace std;

namespace sensord {
    namespace dvld {
        //starts the DVL update loop
        //\param port the Serial port to use
        //\param uInterval the interval between updates
        void start(const string& port, const long uInterval);
    };
};

#endif
