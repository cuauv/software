#include "hydromathd.hpp"
#include "track_iir.hpp"
#include "liquid.h"//also installed fftw.hpp for faster fft (inherited dependency)
#include "udp_receiver.hpp"
#include <sys/time.h>

//received UDP packet from uC
superdongle_packet_t spt;
windowcf w;

windowcf wchA;
windowcf wchB;
windowcf wchC;

windowcf wchA_filtered;
windowcf wchB_filtered;
windowcf wchC_filtered;

windowf wmag_buffer;

iirfilt_crcf track_filterA;
iirfilt_crcf track_filterB;
iirfilt_crcf track_filterC;


double pwr_sum = 0;

nco_crcf nco;
FilterFactory ff;
int local_track_freq = -1;
unsigned long track_sample_idx;
float phase_difference(float phase_a, float phase_b);

std::complex<float> spectrum_fft_output [SPECTRUM_FFT_LENGTH];
float spectrum_fft_magnitude [SPECTRUM_FFT_LENGTH/2];

int daemon_start_time;
timeval tv_daemon_start_time, tv_ping_time;
double next_ping_time;

void do_track(){

    double ping_time;

    if (local_track_freq != shm_settings.track_frequency_target) {
       if (ff.sosMap.count(shm_settings.track_frequency_target)) { //frequency key exists
            if (track_filterA != NULL)
                iirfilt_crcf_destroy(track_filterA);
            if (track_filterB != NULL)
                iirfilt_crcf_destroy(track_filterB);
            if (track_filterC != NULL)
                iirfilt_crcf_destroy(track_filterC);

            float *b = &((ff.sosMap[shm_settings.track_frequency_target]->b)[0]);
            float *a = &((ff.sosMap[shm_settings.track_frequency_target]->a)[0]);
            track_filterA = iirfilt_crcf_create_sos(b,a,NUMBER_OF_SECTIONS);
            track_filterB = iirfilt_crcf_create_sos(b,a,NUMBER_OF_SECTIONS);
            track_filterC = iirfilt_crcf_create_sos(b,a,NUMBER_OF_SECTIONS);

            shm_results_track.track_state = 0;

            local_track_freq = shm_settings.track_frequency_target;
            float normalized_frequency = (float) shm_settings.track_frequency_target / (float) SAMPLING_FREQUENCY;
            nco_crcf_set_frequency(nco,2*M_PI*normalized_frequency);
        }
        else { //frequency key does not exist. Trigger error state
            shm_results_track.track_state = -1;
            std::cerr << shm_settings.track_frequency_target << " WARN TRACK FREQ NOT IN SOS TABLE" << std::endl;
        }

        shm_setg(hydrophones_results_track, shm_results_track);
    }

    std::complex<float> *chA_ptr, *chA_filtered_ptr;
    std::complex<float> *chB_ptr, *chB_filtered_ptr;
    std::complex<float> *chC_ptr, *chC_filtered_ptr;

    float *mag_ptr;
    windowcf_read(wchA,&chA_ptr);
    windowcf_read(wchB,&chB_ptr);
    windowcf_read(wchC,&chC_ptr);

    std::complex<float> filtOutA, filtOutB, filtOutC;

    for (int i = 0; i < SAMPLING_DEPTH; i++) {
        iirfilt_crcf_execute(track_filterA,chA_ptr[i],&filtOutA);
        iirfilt_crcf_execute(track_filterB,chB_ptr[i],&filtOutB);
        iirfilt_crcf_execute(track_filterC,chC_ptr[i],&filtOutC);
        
        windowcf_read(wchA_filtered,&chA_filtered_ptr);
        windowcf_read(wchB_filtered,&chB_filtered_ptr);
        windowcf_read(wchC_filtered,&chC_filtered_ptr);

        windowcf_push(wchA_filtered,filtOutA);
        windowcf_push(wchB_filtered,filtOutB);
        windowcf_push(wchC_filtered,filtOutC);

        float b_sqrd = std::pow(std::real(filtOutB),2);

        pwr_sum += b_sqrd;
        windowf_read(wmag_buffer,&mag_ptr);
        pwr_sum -= mag_ptr[0];
        windowf_push(wmag_buffer,b_sqrd);
        
        gettimeofday(&tv_ping_time, NULL); 
        
        double ping_time_sec = (double) (((long int) tv_ping_time.tv_sec) - ((long int) daemon_start_time));
        ping_time = ping_time_sec + (((double) tv_ping_time.tv_usec) / 1.0e6);


        double normalized_pwr = pwr_sum / (double)TRACK_LENGTH;
        if ((normalized_pwr > shm_settings.track_magnitude_threshold &&
            track_sample_idx > (unsigned int)shm_settings.track_cooldown_samples)) {
            next_ping_time += 2.0;

            std::complex<float> dtft_coeff_A = goertzelNonInteger(chA_filtered_ptr,TRACK_LENGTH,shm_settings.track_frequency_target,SAMPLING_FREQUENCY);
            std::complex<float> dtft_coeff_B = goertzelNonInteger(chB_filtered_ptr,TRACK_LENGTH,shm_settings.track_frequency_target,SAMPLING_FREQUENCY);
            std::complex<float> dtft_coeff_C = goertzelNonInteger(chC_filtered_ptr,TRACK_LENGTH,shm_settings.track_frequency_target,SAMPLING_FREQUENCY);
             
            float phaseA = std::arg(dtft_coeff_A);
            float phaseB = std::arg(dtft_coeff_B);
            float phaseC = std::arg(dtft_coeff_C);

            shm_results_track.diff_phase_y = phase_difference(phaseC,phaseB);
            shm_results_track.diff_phase_x = phase_difference(phaseA,phaseB);

            float kx = SOUND_SPEED * shm_results_track.diff_phase_x / (NIPPLE_DISTANCE * 2 * M_PI * shm_settings.track_frequency_target);
            float ky = SOUND_SPEED * shm_results_track.diff_phase_y / (NIPPLE_DISTANCE * 2 * M_PI * shm_settings.track_frequency_target);

            float kz_2 = 1 - kx * kx - ky * ky;
            if (kz_2 < 0) {
              std::cerr << "WARNING: z mag is negative! " << kz_2 << std::endl;
              kz_2 = 0;
            }


            shm_results_track.daemon_start_time = daemon_start_time;
            shm_results_track.tracked_ping_time = ping_time;
            shm_results_track.tracked_ping_heading_radians = std::atan2(ky, kx);
            shm_results_track.tracked_ping_elevation_radians = std::acos(std::sqrt(kz_2));

            shm_results_track.tracked_ping_count++;
            shm_results_track.tracked_ping_frequency = shm_results_spectrum.most_recent_ping_frequency;  
            shm_results_track.tracked_ping_idx = track_sample_idx; 

            std::cout << "PING DETECTED @ n=" << track_sample_idx << " w/ pwr="<< normalized_pwr<< std::endl;
            std::cout << "@ HEADING=" << shm_results_track.tracked_ping_heading_radians*(180.0f/M_PI) << std::endl; 

            shm_setg(hydrophones_results_track, shm_results_track);
            track_sample_idx = 0;
        }

        track_sample_idx++;
    }
}

std::complex<float> goertzelNonInteger(std::complex<float> *x , int N, float ftarget, float fsample) {
    //This algorithim is taken from
    //http://asp.eurasipjournals.springeropen.com/articles/10.1186/1687-6180-2012-56
    //by Rajmic P.
    //It assumes the data in X is all real. It is better than doing a DFT coefficient beause it allows
    //for non integer DTFT coefficients.
    float k = ftarget/fsample*(float)N; //Frequency index of desired DTFT coeff (not necessarily integer)
    float A = 2*M_PI*k/N;
    float B = 2*cos(A);
    std::complex<float> C = std::exp(std::complex<float>(0.f,-1.0f)*A);
    std::complex<float> D = std::exp(std::complex<float>(0.f,-1.0f)*A * (float)(N-1));

    float s0 = 0;
    float s1 = 0;
    float s2 = 0;

    for(int n = 0; n < N-2; n++)
    {
        s0 = std::real(x[n]) + B*s1 - s2;
        s2 = s1;
        s1 = s0;
    }
    s0 = std::real(x[N-1]) + B*s1-s2;
    std::complex<float> y;
    y = s0 - s1*C;
    y = y*D;
    return y;
}

float phase_difference(float phase_a, float phase_b) {
	float diff = phase_a - phase_b;
	while (diff > M_PI) {
		diff -= 2*M_PI;
	}
	while (diff < -1*M_PI) {
		diff += 2*M_PI;
	}

	return diff;
}

void do_spectrum() {
    std::complex<float> *data_in;
    windowcf_read(w, &data_in);
    fftplan q = fft_create_plan(SPECTRUM_FFT_LENGTH,data_in,spectrum_fft_output,LIQUID_FFT_FORWARD,0);

    fft_execute(q);
    fft_destroy_plan(q);

    spectrum_fft_magnitude[0] = 0.0; //Do not consider DC comp

    int maxIdx = 0;
    float maxVal = 0;
    double total_power = 0;
    double psd = 0;
    for (int i = 1; i < SPECTRUM_FFT_LENGTH/2; i++) {
        spectrum_fft_magnitude[i] = std::norm(spectrum_fft_output[i]);
        total_power += spectrum_fft_magnitude[i];
        if (spectrum_fft_magnitude[i] > maxVal) {
            maxIdx = i;
            maxVal = spectrum_fft_magnitude[i];
        } 
    }
    // TODO normalize so window size does not affect this threshold.
    psd = maxVal / total_power;

    if (shm_settings.auto_magnitude_thresholding == 0) {
        if (psd > shm_settings.spectrum_magnitude_threshold && (current_sample_count - old_spectrum_sample_count > shm_settings.spectrum_cooldown_samples)) {
            shm_results_spectrum.most_recent_ping_magnitude = maxVal;
            shm_results_spectrum.most_recent_ping_frequency = (double) SAMPLING_FREQUENCY / (double) SPECTRUM_FFT_LENGTH * (double) maxIdx;
            shm_results_spectrum.most_recent_ping_count++;
            old_spectrum_sample_count = current_sample_count;
            shm_setg(hydrophones_results_spectrum, shm_results_spectrum);
        }
        prev_psd = psd;
    }
    else {
        //IMPLEMENTE AUTO THRESHOLDING AND ASSOCIATED PING STUFF
    } 
}


int main (int argc, char ** argv) {
    w=windowcf_create(SPECTRUM_FFT_LENGTH);
    wchA = windowcf_create(TRACK_LENGTH);
    wchB = windowcf_create(TRACK_LENGTH);
    wchC = windowcf_create(TRACK_LENGTH);

    wchA_filtered = windowcf_create(TRACK_LENGTH);
    wchB_filtered = windowcf_create(TRACK_LENGTH);
    wchC_filtered = windowcf_create(TRACK_LENGTH);

    wmag_buffer = windowf_create(TRACK_LENGTH);

    nco = nco_crcf_create(LIQUID_NCO);
    std::cout << "hydromath daemon is beginning..." << std::endl;
    std::cout << "NOTE: if this is symhydromath you must pass in a matfile" << std::endl;

    std::cout << argv[1] << std::endl;
    shm_init();
    shm_getg(hydrophones_results_track, shm_results_track);
    shm_getg(hydrophones_results_spectrum, shm_results_spectrum);

    //shm_results_track.tracked_ping_count=0;
    shm_results_track.tracked_ping_time=0; 


    if (argc > 1) {
        udp_init(argv[1]);
    }
    else {
        udp_init("");
    }
    //std::thread track_thread(direction_loop);
    //
    //std::thread spectrum_thread(spectrum_loop);
    track_sample_idx = 0;  

    gettimeofday(&tv_daemon_start_time, NULL);
    daemon_start_time = (int) tv_daemon_start_time.tv_sec;
    next_ping_time = 2.0;

	shm_getg(hydrophones_status, shm_status);
    while (loop(&spt) == 0) {
        shm_getg(hydrophones_settings, shm_settings);
		for (int i = 0; i < 3*CHANNEL_DEPTH; i+=3) {
            windowcf_push(w,    std::complex<float>(spt.data[i+1],0)); //This uses channel B

            windowcf_push(wchA, std::complex<float>(spt.data[i],0));
            windowcf_push(wchB, std::complex<float>(spt.data[i+1],0));
            windowcf_push(wchC, std::complex<float>(spt.data[i+2],0));
        }
        current_sample_count+=CHANNEL_DEPTH;
		packet_count++;
		shm_status.packet_count = packet_count;
        shm_setg(hydrophones_status, shm_status);
		do_spectrum();
        do_track();
    }

    printf("loop done %li =ci\n",current_sample_count/CHANNEL_DEPTH);
    return 0;
}
