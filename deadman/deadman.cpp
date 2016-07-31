/*
 * Network Deadman Switch for sub
 * CUAUV
 * Created: Benjamin Seidenberg (bes36), Sept. 2008
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>

#include <libshm/c/shm.h>

//#define FORK_MODE

using namespace std;

const char base_command[]="ping -q -i 10 -w 1 ";
const char tail_command[]=" >/dev/null";

pid_t child = 0;

struct deadman_opts {
    int timeout;
    char *host;
};

deadman_opts opts = {
    10,
    NULL
};

void die(int sig) {
    if (sig != SIGTERM && sig != SIGINT)
    	return;
    if (child) {
        kill(child, SIGTERM);
     }
     exit(0);
}
    

void usage() {
    printf("auv-deadman HOST [INTERVAL]\n");
    printf("auv-deadman sends a ping to HOST every second.\n");
    printf("If INTERVAL seconds (default 10) go by with no\n");
    printf("response, auv-deadman breaches the vehicle.\n");
}

int main(int argc, char **argv) {
    signal(SIGINT,die);
    signal(SIGTERM,die);
    signal(SIGHUP,die);
    // Parse args here - Eventually switch to getopts
    if (argc < 2 || argc > 3) {
        usage();
        return 1;
    }
    opts.host=argv[1];
    if (argc > 2)
        opts.timeout=atoi(argv[2]);
    // Sanity check args here
    if (opts.host == NULL)
        return 1;
    // FIXME - Really do sanity checking
    char *command = (char *)malloc(strlen(base_command)+strlen(opts.host)+strlen(tail_command) +1);
    strcpy(command,base_command);
    strcat(command,opts.host);
    strcat(command,tail_command);

    shm_init();

    printf("Testing for connection...");
    if (system(command)) {
        printf("failed!\nAborting!\n");
        return 1;
    }
    else printf("success!\nSwitch enabled...\n");

    #ifdef FORK_MODE
    child=fork();
    #endif

    int count=0;
    #ifdef FORK_MODE
    if (child) {
        wait(NULL);
        return 0;
    }
    else
    #endif
        while(count < opts.timeout) {
            count = system(command) ? count+1 : 0;
        }
    // If we got here, breach sub and try to keep motors to a minimum
    int status;
    printf("BREACH!!!!\n");
    status = system("pkill -f mission"); //Stop any mission that might be running
    if (!status) {
        printf("Uh oh, couldn't kill mission!");
    }
    shm_set(switches, soft_kill, false);
    shm_set(mission_start_switch, mission_start, false);
    shm_set(navigation_desires, depth, 0);
    shm_set(navigation_desires, speed, 0);
    shm_set(navigation_desires, sway_speed, 0);
    shm_set(settings_control, pitch_active, false);
    // XXX: May leave port/star/sway thrusters in a bad state if turned off
    //shm_set(settings_control, heading_active, false);

    
    //Strop any existing led flashes and engage strobe!
    status = system("pkill -f led");
    if (!status) {
        printf("Uh oh, couldn't kill led!");
    }
    status = system("auv-led-strobe");
    if (!status) {
        printf("Uh oh, couldn't engage strobe!");
    }

}

