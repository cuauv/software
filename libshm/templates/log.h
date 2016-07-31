#ifndef LOG_LOG_H
#define LOG_LOG_H

#include "shm.h"

#include <iostream>
#include <fstream>

using namespace std;

#ifdef __cplusplus
extern "C" {
#endif

#define TYPE_STRING_OLD     0x01
#define TYPE_INT            0x02
#define TYPE_DOUBLE         0x03
#define TYPE_STRING         0x04

void shmlog_table(ofstream* out);

void shmlog_snapshot();

void shmlog_modified(ofstream* out);

void shmlog_time(ofstream* out);

#ifdef __cplusplus
}
#endif

#endif  // LOG_LOG_H
