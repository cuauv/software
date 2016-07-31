// ASLAM Daemon
// Written by Christopher Goes (cwg46@cornell.edu)

// Last Update 5 May 2014

#include "aslam.h"

#include <iostream>
#include <cstring>
#include <time.h>

using std::cin;
using std::cout;
using std::endl;
using std::shared_ptr;

static void log(string s) {
    time_t rtime;
    tm* tinfo;
    char b[100];

    time(&rtime);
    tinfo = gmtime(&rtime);
    strftime(b, 100, "[%Y.%m.%d %H:%M:%S UTC] ", tinfo);

    cout << b << s << endl;
}

int main() {
    log("ASLAM daemon launched");
    
    log("Initializing state...");

    // TODO: Read SHM
    shared_ptr<State> s (new State(0, 0, 1));

    log("Polling SHM...");
    while(true) {
    }
    
}

/* SHM:
ObjManip:
Action - String: add / update / remove
Identifier - String
Heading - Float
Distance - Float
Uncertainty - Float
Submit - Boolean

Settings:
sub_init_location_n: Float
sub_init_location_e: Float
sub_init_location_unc: Float
location_source: Kalman / DVL / ?
time_decay_coefficient - Float
heading_reject_threshold_flat - Float
distance_reject_threshold_pct - Float
ex_enable_adaptive_error - Boolean

Results:
objects:
[id] -> north, east, uncertainty
heading_error_offset - Float
heading_error_linear - Float
distance_error_offset - Float
distance_error_linear - Float
*/
