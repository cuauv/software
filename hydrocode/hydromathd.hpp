#ifndef __HYDROMATHD_HPP__
#define __HYDROMATHD_HPP__

#include <iostream>
#include <cstdint>
#include <ctime>
#include "udp_receiver.hpp"
#include <cmath>
#include <complex>
#include <thread>
#include "libshm/c/shm.h"

#define buffer_0_0 buffer_B
#define buffer_0_1 buffer_C
#define buffer_1_0 buffer_A

struct hydrophones_settings shm_settings;
struct hydrophones_status shm_status;
struct hydrophones_results_spectrum shm_results_spectrum;
struct hydrophones_results_track shm_results_track;

const int SAMPLING_FREQUENCY = 400000;
const int SAMPLING_DEPTH = 128;
const int SPECTRUM_FFT_LENGTH = 1024; //Must be a power of 2
const int TRACK_LENGTH = 256;

const float SOUND_SPEED = 1481.0; // in water at 20 Degrees Celsius
const float NIPPLE_DISTANCE = 0.013; // distance between transducers (right angles) in meters

double prev_psd = 0;
long current_sample_count = 0;
long old_track_sample_count = 0;
long old_spectrum_sample_count = 0;
int packet_count = 0;

//Direction_thread --> using IIR for detection and NI-Goertzel for phase
void direction_loop();

//Spectrum_thread --> using FFT 
void spectrum_loop();

std::complex<float> goertzelNonInteger(std::complex<float> *, int, float, float);
#endif //__HYDROMATHD_HPP__
