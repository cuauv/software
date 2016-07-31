#include <iostream>
#include <list>
#include <map>
#include <signal.h>

#include <libshm/c/shm.h>

#include "autodetector.h"
#include "devices/devices.h"
#include "devices/testDevice.h"
#include "device.h"
#include "pollblock.h"

using namespace std;

namespace {
enum {
    TYPE_INT16,
    TYPE_INT32,
    TYPE_BOOL,
    TYPE_FLOAT,
    TYPE_STRING
};

// Need this because abortAll must only take an int argument
list<Device*> devices;
map<int, Device*> autoDetectDevices;

// Ports that are NOT auto-detect:
// 6: DVL Gemini 2014
// 8: 3DMG Gemini 2014
// 11: Trax Gemini 2014
// 12: Battery pod (podd)
// 14: Battery pod (podd)

const char* autoDetectPorts[] = {
//    "/dev/ttyUSB_thor_POD",
//    "/dev/ttyUSB_thor_PD",
//    "/dev/ttyUSB_thor_GPIO",
//    "/dev/ttyUSB_thor_ACT",
    "/dev/ttyUSB_thor_HIM",
//    "/dev/ttyUSB_thor_THRUST",
};

const size_t numAutoDetectPorts = sizeof(autoDetectPorts) / sizeof(autoDetectPorts[0]);

}

void abortAll(int signal) {
    list<Device*>::iterator it;
    for (it = devices.begin(); it != devices.end(); ++it) {
        (*it)->stop();
        delete *it;
    }

    map<int, Device*>::iterator it2;
    for (it2 = autoDetectDevices.begin(); it2 != autoDetectDevices.end(); ++it2) {
        it2->second->stop();
        delete it2->second;
    }
}

void dumpAll(int signal) {
    list<Device*>::iterator it;
    for (it = devices.begin(); it != devices.end(); ++it) {
        (*it)->dump();
    }

    map<int, Device*>::iterator it2;
    for (it2 = autoDetectDevices.begin(); it2 != autoDetectDevices.end(); ++it2) {
        it2->second->dump();
    }
}

#include <signal.h>
#include <signal.h>
#include <execinfo.h>
#include <sys/resource.h>
#include <popt.h>
#include <exception>

#define BT_REG REG_RIP

// Stacktrace struct
struct sig_ucontext_t {
    unsigned long   uc_flags;
    ucontext        *uc_link;
    stack_t         uc_stack;
    sigcontext      uc_mcontext;
    sigset_t        uc_sigmask;
};

// Backtrace on segfault
void HandleSigSegv(int sig_num, siginfo_t *info, void *ucontext) {
    void *array[50];
    void *caller_address;
    char **messages;
    int size;
    ucontext_t *uc;

    uc = (ucontext_t*)ucontext;

    caller_address = (void*)uc->uc_mcontext.gregs[BT_REG];

    fprintf(stderr, "signal %d (%s), address is %p from %p", sig_num, strsignal(sig_num), 
          info->si_addr, (void*)caller_address);

    size = backtrace(array, 50);
    array[1] = caller_address;
    messages = backtrace_symbols(array,size);
    
    for (int i=1; i<size && messages != NULL; ++i) {
        fprintf(stderr, "[bt]: (%d) %s", i, messages[i]);
    }
    free(messages);
    exit(-1);
}

void HandleException( ) {

    void *trace_elems[20];
    int trace_elem_count(backtrace(trace_elems, 20));
    char **stack_syms(backtrace_symbols(trace_elems, trace_elem_count));
    for( int i = 0 ; i < trace_elem_count ; ++i )
    {
        fprintf(stderr, "[bt]: (%d) %s", i, stack_syms[i]);
    }

    exit(-1);
};

void usage(char* name) {
    cout << "usage: " << name << " [-v] config-file" << endl;
}

int main(int argc, char** argv) {
    signal(SIGINT, abortAll);
    signal(SIGTERM, abortAll);
    signal(SIGHUP, abortAll);
    signal(SIGUSR1, dumpAll);

    struct sigaction sigact = {{0}};
    sigact.sa_sigaction = HandleSigSegv;
    sigact.sa_flags = SA_RESTART | SA_SIGINFO;
    sigaction(SIGSEGV, &sigact, (struct sigaction*)NULL);
    std::set_terminate(HandleException);

    shm_init();

    if (argc > 2 && strncmp(argv[1], "-t", 3) == 0) {
        TestDevice* td = new TestDevice(0x3412, "test", argv[2], false, false);
        devices.push_back(td);
    }
    else {
        initDevices(devices, autoDetectDevices);
    }

    if (devices.size() + autoDetectDevices.size() == 0) {
        cerr << "Config file did not specify any devices!" << endl;
        exit(1);
    }

    list<Device*>::iterator iter;
    for (iter = devices.begin(); iter != devices.end(); ++iter) {
        (*iter)->start();
        // TODO: check for protocol version, other stuff.
    }

    if (autoDetectDevices.size() > 0) {
        Autodetector(numAutoDetectPorts, autoDetectPorts, autoDetectDevices);
    }

    for (iter = devices.begin(); iter != devices.end(); ++iter) {
        (*iter)->join();
    }
    map<int, Device*>::iterator it2;
    for (it2 = autoDetectDevices.begin(); it2 != autoDetectDevices.end(); ++it2) {
        it2->second->join();
    }
}

