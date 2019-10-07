//
//  comms.hpp
//  hydromathd
//
//  Created by Vlad on 2/5/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#ifndef COMMS_HPP
#define COMMS_HPP

#include <complex>

#include "liquid.h"

enum downconverter_status{DOWNCONV_DEFAULT, NEW_CONV_SAMPLE};
enum synchronizer_status{SYNCH_DEFAULT, NEW_SYNCH_POINT, SYNCH_LOCKED_ON};
enum decider_status{DECID_DEFAULT, NEW_SYMBOL};
enum packer_status{PACKER_DEFAULT, PKT_FULL};

class DCRemover
{
public:
    DCRemover(unsigned int len);
    ~DCRemover(void);
    float push(float in_sample);
private:
    unsigned int len;
    float avg;
    windowf buff;
};

class StdDev
{
public:
    StdDev(unsigned int len);
    ~StdDev(void);
    float push(float in_sample);
private:
    unsigned int len;
    float std_dev;
    windowf buff;
};

class Downconverter
{
public:
    Downconverter(unsigned int in_sample_rate, unsigned int decim_factor, unsigned int carrier_freq, unsigned int bandwidth);
    ~Downconverter(void);
    void push(float in_sample);
    downconverter_status getStatus(void);
    std::complex<float> getSample(void);
private:
    static const unsigned int TRANS_WIDTH;
    static const unsigned int STOPBAND_ATTEN;
    
    downconverter_status status;
    unsigned int n;
    unsigned int decim_factor;
    std::complex<float> conv_sample;
    
    nco_crcf mix_osc;
    firfilt_crcf filt;
};

class FSKSynchronizer
{
public:
    FSKSynchronizer(unsigned int sample_rate, const int symbols[2], unsigned int sym_rate, const float *sym_corrections, const float code[], const float orth_code[], unsigned int code_len);
    ~FSKSynchronizer(void);
    void push(std::complex<float> in_sample);
    synchronizer_status getStatus(void);
    windowf dumpCorrInBuff(void);
    windowf dumpCorrSigOutputBuff(void);
    windowf dumpCorrOrthOutputBuff(void);
    windowf dumpDynThreshBuff(void);
    void rst(void);
private:
    static const std::complex<float> j;
    static const unsigned int BUFF_LEN;
    static const unsigned int SYM_WIDTH;
    static const unsigned int TRANS_WIDTH;
    static const unsigned int STOPBAND_ATTEN;
    static const unsigned int SAMPLES_PER_SYMBOL;
    static const float THRESH_CALC_LEN;
    static const float THRESH_FACTOR;
    
    synchronizer_status status;
    bool triggered;
    unsigned int n, trigger_n;
    unsigned int sample_rate;
    unsigned int sym_rate;
    const float *sym_corrections;
    unsigned int code_len;
    float corr_sig_peak;
    
    windowf corr_in_buff, corr_sig_output_buff, corr_orth_output_buff, dyn_thresh_buff;
    firfilt_cccf space_filt, mark_filt;
    firfilt_rrrf corr_sig, corr_orth;
    StdDev thresh_calc;
};

class FSKDecider
{
public:
    FSKDecider(unsigned int sample_rate, unsigned int bits_per_sym, const int symbols[], const float *sym_corrections, unsigned int sym_rate);
    ~FSKDecider(void);
    void push(std::complex<float> in_sample);
    decider_status getStatus(void);
    unsigned int getSym(void);
    void rst(void);

private:
    static const std::complex<float> j;
    static const unsigned int SYM_WIDTH;
    static const unsigned int TRANS_WIDTH;
    static const unsigned int STOPBAND_ATTEN;
    static const unsigned int SAMPLES_PER_SYMBOL;
    
    decider_status status;
    unsigned int n;
    unsigned int num_accum_samples;
    unsigned int sample_rate;
    unsigned int sym_rate;
    unsigned int num_sym;
    const float *sym_corrections;
    float *energies;
    unsigned int highest_energy_sym;
    
    firfilt_cccf *filters;
};

class SymPacker
{
public:
    SymPacker(unsigned int pkt_size, unsigned int bits_per_sym);
    ~SymPacker(void);
    void push(unsigned int sym);
    packer_status getStatus(void);
    unsigned char *getPkt(void);
    void rst(void);

private:
    packer_status status;
    unsigned int sym_num;
    unsigned int bits_per_sym;
    unsigned int pkt_size;
    unsigned char *sym_buff;
    unsigned char *pkt;
};

#endif /* comms_hpp */
