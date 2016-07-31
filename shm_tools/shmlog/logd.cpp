#include <stdio.h>
#include <ctime>
#include <libshm/c/log.h>
#include <libshm/c/shm.h>
#include <popt.h>
#include <iostream>
#include <fstream>
#include <sys/time.h>
#include <unistd.h>
#include <csignal>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <vector>

using namespace std;

uint32_t magic_number = 0x6C565541; //AUVl;

struct app_options {
    char* filename;
    int frequency;
    char* name;
    int vision_tag_enable;
};

struct snapshot {
    uint64_t filepos;
    uint32_t tv_sec;
    uint32_t tv_usec;
};

app_options opts = {
    (char*) "",
    200, //logging frequency should be faster than any daemon
    (char*) "untitled",
    0
};

poptOption app_options_table[] = {
    {"filename", 'o', POPT_ARG_STRING, &opts.filename, 0, "log to this file (omit to use default location)", "FILENAME"},
    {"frequency", 'f', POPT_ARG_INT, &opts.frequency, 0, "log at this frequency in hz (omit to use default of 200)", "FREQ"},
    {"tagname", 't', POPT_ARG_STRING, &opts.name, 0, "specify a \"tag name\" for this log's filename (omit for no tag name)", "TAG"},
    {"vision_tag_enable", 'v', POPT_ARG_NONE, &opts.vision_tag_enable, 0, "if specified, vision tagging will be started with the same tag name", "VISION_TAG_ENABLE"},
    POPT_AUTOHELP
    POPT_TABLEEND
};

int keep_going = 1;
void interrupt_handler(int) {
    cout << "INTERRUPT" << endl;
    keep_going = 0;
}

int main(int argc, char *argv[]){

    shm_init();

    poptContext argCtxt = poptGetContext(NULL, argc, (const char**)argv, app_options_table, 0);

    if(poptGetNextOpt(argCtxt) < -1) {
        fprintf(stderr, "Error: Bad option '%s' specified\n", poptBadOption(argCtxt, 0));
        poptPrintHelp(argCtxt, stdout, 0);
        poptFreeContext(argCtxt);
        return -1;
    }

    poptFreeContext(argCtxt);

    long wait_us = 1e6 / opts.frequency;

    int snapshot_s = 5; //snapshot every x seconds; reduce for quicker seeks, but more space
    timeval t1, t2; //variables for snapshot measurement
    t1 = (struct timeval){0};

    //Enable vision tagging
    if(opts.vision_tag_enable){
       shm_setstr(camera, image_tag, opts.name);
       puts("Tagging vision...");
    }

    vector<snapshot*> snapshots;

    time_t raw;
    time(&raw);
    static tm* timeinfo;
    timeinfo = localtime(&raw);

    static char timeBuf[256];
    static char timeBufDir[65];
    static char timeBufFile[65];
    static char timeBufFileTime[65];

    strftime( timeBuf, 255, "%a %b %d %Y %X", timeinfo );
    strftime( timeBufDir, 64, "/auv/pooltest-%Y-%m-%d", timeinfo );
    strftime( timeBufFileTime, 64, "%H.%M.%S", timeinfo );
    sprintf(timeBufFile, "log-%s-%s.shmlog", opts.name, timeBufFileTime);

    mkdir(timeBufDir, 0777); //Make directory if it does not exist

    static char outputFileName[128];
    if(strlen(opts.filename)){
        strcpy(outputFileName, opts.filename); //use input filename
    }else{
        strcpy(outputFileName, timeBufDir); //construct default filename
        strcat(outputFileName, "/");
        strcat(outputFileName, timeBufFile);
    }

    ofstream out(outputFileName); //Start log file output
    if( !out ){
        cout << "Couldn't open file " << outputFileName << " for writing."  << endl;
        return 1;
    }

    cout << "Writing to file " << outputFileName << " at " << opts.frequency << " HZ" << endl;

    char buf[32];
    gethostname(buf,sizeof buf);

    out.write(reinterpret_cast<char*>(&magic_number), sizeof(uint32_t)); //Write magic number for file format

    out << "Shared Memory log on " << buf<< " at " << timeBuf << " (" << opts.frequency << " HZ)" << endl;

    signal(SIGINT, interrupt_handler);
    signal(SIGTERM, interrupt_handler);

    shm_init();

    shmlog_table(&out); //Write the shm table to log file

    //shmlog_snapshot(); //Take a snapshot of shared memory

    uint16_t dx = 0xFFFF; //Separation constant
    out.write(reinterpret_cast<char*>(&dx), sizeof(uint16_t)); //Separation between table and log

    //Log continuously
    while(keep_going){
        gettimeofday(&t2, NULL);
        if(t2.tv_sec - t1.tv_sec >= snapshot_s){ //Take snapshot
            shmlog_snapshot();
    
            struct snapshot* snp = (struct snapshot*) malloc(sizeof(struct snapshot));
            snp->filepos = out.tellp();
            snp->tv_sec = t2.tv_sec;
            snp->tv_usec = t2.tv_usec; 
            
            snapshots.push_back(snp); //record snapshot

            t1 = t2;
        }
        
        shmlog_modified(&out);
        usleep(wait_us);
    }
    
    cout << "Finalizing log file..." << endl;

    out.write(reinterpret_cast<char*>(&dx), sizeof(uint16_t)); //end of var data
    
    shmlog_time(&out); //Write final timestamp for end of log

    //Mark the start of the snapshot table
    uint64_t max64 = 0xFFFFFFFFFFFFFFFFull;
    uint32_t zero = 0;
    out.write(reinterpret_cast<char*>(&max64), sizeof(uint64_t)); //TODO: Make this section better
    out.write(reinterpret_cast<char*>(&zero), sizeof(uint32_t));
    out.write(reinterpret_cast<char*>(&zero), sizeof(uint32_t));
    
    //write snapshot table to the file
    vector<snapshot*>::reverse_iterator it;
    for( it = snapshots.rbegin() ; it < snapshots.rend() ; it++){
        snapshot* snp = *it;
        out.write(reinterpret_cast<char*>(&snp->filepos), sizeof(uint64_t));
        out.write(reinterpret_cast<char*>(&snp->tv_sec), sizeof(uint32_t));
        out.write(reinterpret_cast<char*>(&snp->tv_usec), sizeof(uint32_t));
    }

    out.close();

    puts("log file written!");

    //Disable vision tagging
    if(opts.vision_tag_enable){
       opts.name = (char*) "";
       shm_setstr(camera, image_tag, opts.name);
       puts("Vision tagging stopped.");
    }


    return 0;

}
