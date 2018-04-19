#include <cstdlib>

#include <boost/smart_ptr.hpp>
#include <libauvstate-cpp/cuauv/cpp/shmsharedvar.h>
#include <libauvstate-cpp/cuauv/cpp/shmvarwatch.h>

#include "killsubmarine.hpp"

extern "C" {

void KillSubmarine() {
    AUV::ShmSharedVar<bool>("/settings/switches/soft_kill") = true;
}

void UnkillSubmarine() {
    AUV::ShmSharedVar<bool>("/settings/switches/soft_kill") = false;
}

}

