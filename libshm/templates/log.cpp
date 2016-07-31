#include "log.h"
#include <sys/time.h>

timeval tim;
uint32_t sec;
uint32_t usec;
uint16_t tflg = 0xFFFE; 
char vartype;
const char RS = (char) 30;

void shmlog_time(ofstream* out){
    gettimeofday(&tim, NULL);
    sec = (uint32_t) tim.tv_sec;
    usec = (uint32_t) tim.tv_usec;
    (*out).write(reinterpret_cast<char*>(&tflg), sizeof(uint16_t));
    (*out).write(reinterpret_cast<char*>(&sec), sizeof(uint32_t));
    (*out).write(reinterpret_cast<char*>(&usec), sizeof(uint32_t));
}

void shmlog_table(ofstream* out){
    uint16_t i = 0;
    <!--(for g in groups)-->
        <!--(for k in g['varnames'])-->    
    
            <!--(if g['vars'][k]['type'] == 'string')-->
    vartype = TYPE_STRING;
            <!--(elif g['vars'][k]['type'] == 'double')-->
    vartype = TYPE_DOUBLE;
            <!--(elif g['vars'][k]['type'] == 'float')-->
    vartype = TYPE_DOUBLE; //Will cast float to type double
            <!--(else)-->
    vartype = TYPE_INT;
            <!--(end)-->
    (*out).write(reinterpret_cast<char*>(&i), sizeof(uint16_t)); i++;
    (*out).write(reinterpret_cast<char*>(&vartype), sizeof(char));
    (*out) << "$!g['groupname']!$.$!k!$" << RS;
        <!--(end)-->
    <!--(end)-->
}

//sets all variables' flags
void shmlog_snapshot(){
    <!--(for g in groups)-->
    shm->$!g['groupname']!$.m.f = 1;
    <!--(end)-->
}

void shmlog_modified(ofstream* out){
    uint16_t i = 0;
    uint32_t var_length = 0;
    double temp = 0;
    temp = temp; //lol, werror
    
    if(<!--(for g in groups)-->shm->$!g['groupname']!$.m.f || <!--(end)-->0){
        //something has changed; make a new timeslice 
        shmlog_time(out);
    }else{
        return;
    }

    <!--(for g in groups)-->
    if(shm->$!g['groupname']!$.m.f){
   
        struct $!g['groupname']!$ gt;
        
        shm_lock($!g['groupname']!$);
        gt = shm->$!g['groupname']!$.g; 
        shm->$!g['groupname']!$.m.f = 0;
        shm_unlock($!g['groupname']!$);

        <!--(for k in g['varnames'])-->    
        (*out).write(reinterpret_cast<char*>(&i), sizeof(uint16_t));
            <!--(if g['vars'][k]['type'] == 'string')-->
        var_length = strlen(gt.$!k!$);
        (*out).write(reinterpret_cast<char*>(&var_length), sizeof(uint32_t)); //preface with string length
        (*out) << gt.$!k!$; i++;
            <!--(elif g['vars'][k]['type'] == 'double')-->
        (*out).write(reinterpret_cast<char*>(&gt.$!k!$), sizeof(double)); i++;
            <!--(elif g['vars'][k]['type'] == 'float')-->
        temp = (double) gt.$!k!$; //Cast float to type double
        (*out).write(reinterpret_cast<char*>(&temp), sizeof(double)); i++;
            <!--(else)-->
        (*out).write(reinterpret_cast<char*>(&gt.$!k!$), sizeof(int32_t)); i++;
            <!--(end)-->
        <!--(end)-->
    }else{
        i += @!len(g['vars'])!@;
    }
    <!--(end)-->
}
