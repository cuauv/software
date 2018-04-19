#include <cstdlib>
#include <inttypes.h>
#include <libshm/c/shm.h>
#include "pod_pkt.h"
#include "../../serial/serial.h"

using namespace std;
using namespace sensorserial;

#define DEBUG_PRINT 1

#define VU 	0.02928		//V
#define IU 	-0.00078125	//A
#define TU 	0.125		//deg C
#define ACRU 	3.125 		//mAh

#define EMPTY	29952		//adc units
#define FULL 	1280			//adc units

#define CHGTF_MASK		0x80
#define LOW_BATT_MASK	0x01
#define DCHG_EN_MASK	0x10

#define STBD 1
#define PORT 2

#define QUERY_V	0x12		//Asks for all data (verbose state)
#define CRIT_POD 0x13
#define NOT_CRIT_POD 0x14
#define QUERY_READ_LEN	15
#define DEVICE_TYPE	0x50		// Board's device type byte 'P'
#define DEVICE_ID	0x00		// Board's device ID byte
#define WRITE_LEN	5		//Data field has length 0
#define MIN_PKT_LEN 5		//Null data field length


void write_packet(SerialPort *sp, uint8_t cmd) {
    //Create a zero-data-length packet by brute force
    unsigned char checksum = DEVICE_TYPE + DEVICE_ID + cmd;
    unsigned char pkt[WRITE_LEN];
    pkt[0] = DEVICE_TYPE;
    pkt[1] = DEVICE_ID;
    pkt[2] = cmd;
    pkt[3] = 0x00;
    pkt[4] = checksum;
    sp->flushBuffers();
    sp->writeSer((const unsigned char*)pkt, WRITE_LEN);
}

uint8_t read_packet(SerialPort *sp, pod_pkt *pp, uint8_t cmd, uint8_t readLen) {
    ssize_t size = sp->readnWithTimeout((unsigned char*)pp, readLen, 500000);
#if(DEBUG_PRINT)
    printf ("Recieved %d bytes\n", (int)size);
#endif
    //Filters timeout
    if(size<MIN_PKT_LEN) {
#if(DEBUG_PRINT)
        fprintf(stderr, "Not even a packet.\n");
#endif
        return 0;
    }
    //Tally checksum
    uint8_t checksum = 0;
    uint8_t* i = (uint8_t*)pp;
    uint8_t* istop = i + size - 1;
    while(i<istop) {
        checksum += *i;
        i++;
    }
    //Now i points to checksum field of packet
    //Filters corrupted packets
    if(checksum != *i) {
#if(DEBUG_PRINT)
        fprintf(stderr, "Bad checksum: %d, Calculated: %d.\n", *i, checksum);
#endif
        return 0;
    }
    //Filters error packets: could be more here for complex errors
    if(pp->mesg_type != cmd) {
#if(DEBUG_PRINT)
        fprintf(stderr, "Error packet received.\n");
#endif
        return 0; 
    }
    //Filters any other messages not formatted properly
    if(size != readLen) {
#if(DEBUG_PRINT)
        fprintf(stderr, "Insufficient packet length.\n");
#endif
        return 0;
    }
    return 1;
}

void process_data(pod_pkt *pp, struct pod_port &pd) {
    double vu = VU;
    double iu = IU;
    double tu = TU;
    double empty = EMPTY;
    double full = FULL;
    //Convert from ADC values to physical units
    pd.voltage = (((double)(pp->volt*vu)));
    pd.current = ((double)(pp->curr*iu));
    pd.capacity = ((int)(ACRU*(pp->acr - empty)));
    pd.temp = ((double)(pp->temp*tu));
    pd.percent = ((int)(100.0*((pp->acr - empty)/full)));
    //Unpack booleans
    pd.low_batt = ((pp->status & LOW_BATT_MASK));
    pd.charge_detected = ((pp->status & CHGTF_MASK));
    pd.discharge_enabled = ((pp->status & DCHG_EN_MASK));
    //Get boardID
    pd.board_id = ((int)(pp->boardID));
}

void print_data(struct pod_port &pd, bool stbd) {
#if (!DEBUG_PRINT)
    return;
#endif
    if(stbd) printf("Starboard Pod:\n");
    else printf ("Port Pod:\n");
    printf("Voltage: %f V\n", 		pd.voltage);
    printf("Current Draw: %f A\n", 		pd.current);
    printf("Temperature: %f deg C\n",	pd.temp);
    printf("Remaining Capacity: %u %%\n", 	pd.percent);
    if(pd.low_batt) 		printf("Low Battery.\n");
    if(pd.discharge_enabled)	printf("Discharge Enabled.\n");
    if(pd.charge_detected) 	printf("Full Charge Detected.\n");
    printf("Board ID: %u\n",		pd.board_id);
    printf("\n");
}

void data_clear(struct pod_port &pd) {
    memset(&pd, 0, sizeof(pd));
}


int main(int argc, const char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: auv-podd [stbd pod serial starboard] [port pod serial port]\n");
        return -1;
    }

    shm_init();

    bool twoPods = (argc > 2);
    int onePodDown = 0;

    //SerialPort serS(argv[1], 19200);
    SerialPort* sps = new SerialPort(argv[1],19200);
    pod_pkt podPktS;
    struct pod_port podDataS;
    data_clear(podDataS);

    pod_pkt podPktP;
    struct pod_port podDataP;
    data_clear(podDataP);

    double total_cap;   //Units of mAh
    double total_curr;  //Unit of A

    SerialPort* spp = NULL;
    if(twoPods) {
        //SerialPort serP(argv[2], 19200);
        spp = new SerialPort(argv[2], 19200);
    }

    if(twoPods) printf("Initialized podd for 2 pods.\n");
    else printf("Initialized podd for 1 pod.\n");

    while(1) {
        //Clear the time remaining accumulator
        total_cap = 0;
        total_curr = 0;

        write_packet(sps, QUERY_V);
        if (read_packet(sps, &podPktS, QUERY_V, QUERY_READ_LEN)) {
            process_data(&podPktS, podDataS);
            total_cap += (int)(podDataS.capacity);
            total_curr += podDataS.current;
            print_data(podDataS, true);
            //Communication re-established
            if (twoPods && (onePodDown == STBD)) {
                printf("Re-established communication with starboard pod.\n");
                write_packet(spp, NOT_CRIT_POD);
                onePodDown = 0;
            }
        }
        //Communication is lost
        else if (twoPods && (!onePodDown)) {
            printf("Lost communication with starboard pod.\n");
            data_clear(podDataS);
            write_packet(spp, CRIT_POD);
            onePodDown = STBD;
        }

        if(twoPods) {
            write_packet(spp, QUERY_V);
            if (read_packet(spp, &podPktP, QUERY_V, QUERY_READ_LEN)) {
                process_data(&podPktP, podDataP);
                total_cap += (int)(podDataP.capacity);
                total_curr += podDataP.current;
                print_data(podDataP, false);
                //Communication re-established
                if(twoPods && (onePodDown == PORT)) {
                    printf("Re-established communication with port pod.\n");
                    write_packet(sps, NOT_CRIT_POD);
                    onePodDown = 0;
                }
            }
            //Communication is lost
            else if (twoPods && (!onePodDown)) {
                printf("Lost communication with port pod.\n");
                data_clear(podDataP);
                write_packet(sps, CRIT_POD);
                onePodDown = PORT;
            }
        }

        // Save the results.
        shm_setg(pod_port, podDataP);
        struct pod_port* stb_ptr = &podDataS;
        shm_setg(pod_starboard, *(struct pod_starboard*)stb_ptr);

        //Convert total curr to mAh and calculate time remaining in hours
        double time_remaining = total_cap/(1000*total_curr);
        if(total_curr != 0)
            shm_set(pods, time_remaining, time_remaining);
#if(DEBUG_PRINT)
        printf ("Time Remaining: %f hrs\n", time_remaining);
#endif
        usleep(1000000);
    }
    delete sps;
    if (twoPods) {
        delete spp;
    }
    return 1;
}
