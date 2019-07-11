//
//  comms.hpp
//  hydromathd
//
//  Created by Vlad on 2/5/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#ifndef comms_hpp
#define comms_hpp

#include <cstdint>
#include <complex>

static const unsigned int carrier_freq = 52'000;
static const unsigned int lowpass_width = 3'000;
static const unsigned int lowpass_transition_width = 500;
static const unsigned int stopband_attenuation = 60;

static const unsigned int dsp_sampling_rate = 10'000;
static const int space_freq = -1'000;
static const int mark_freq = 1'000;
static const unsigned int symbol_width = 500;
static const unsigned int symbol_transition_width = 100;

static const unsigned int bit_rate = 8;
static const unsigned int comms_packet_length = 8;
static const unsigned int corr_resolution = 25;
static const float space_correction_factor = 3.0;
static const float mark_correction_factor = 1.0;
static const unsigned int gold_code_length = 31;
static const bool gold_code[gold_code_length] = {1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0};
static const bool another_gold_code[gold_code_length] = {1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0};
static const float threshold_factor = 3.0;

static const unsigned int dc_averaging_duration = 100'000;
static const unsigned int noise_std_dev_duration = 1'000'000;
static const unsigned int comms_filtered_plot_period = 1'000'000;
static const unsigned int comms_filtered_plot_duration = 1'000'000;
static const unsigned int corr_plot_duration = 5'000'000;

static const unsigned int buffer_length = 65536;
static const std::complex <float> j(0, 1);

void commsInit(void);
void commsReset(void);
void commsDSP(uint16_t *fpga_packet, unsigned int packet_no);

#endif /* comms_hpp */
