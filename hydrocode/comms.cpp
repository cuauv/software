//
//  comms.cpp
//  hydromathd
//
//  Created by Vlad on 2/5/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#include <cstdint>
#include <complex>
#include <cstring>

#include "liquid.h"
//#include "libshm/c/vars.h"
#include "comms.hpp"

const unsigned int Downconverter::TRANS_WIDTH = 500;
const unsigned int Downconverter::STOPBAND_ATTEN = 60;

const std::complex<float> FSKSynchronizer::j = {0, 1};
const unsigned int FSKSynchronizer::BUFF_LEN = 65536;
const unsigned int FSKSynchronizer::SYM_WIDTH = 500;
const unsigned int FSKSynchronizer::TRANS_WIDTH = 100;
const unsigned int FSKSynchronizer::STOPBAND_ATTEN = 60;
const unsigned int FSKSynchronizer::SAMPLES_PER_SYMBOL = 25;
const float FSKSynchronizer::THRESH_CALC_LEN = 200;
const float FSKSynchronizer::THRESH_FACTOR = 3.0;

const std::complex<float> FSKDecider::j = {0, 1};
const unsigned int FSKDecider::SYM_WIDTH = 500;
const unsigned int FSKDecider::TRANS_WIDTH = 100;
const unsigned int FSKDecider::STOPBAND_ATTEN = 60;
const unsigned int FSKDecider::SAMPLES_PER_SYMBOL = 25;

DCRemover::DCRemover(unsigned int len):
len(len),
avg(0),
buff(windowf_create(len))
{
}
DCRemover::~DCRemover(void)
{
    windowf_destroy(buff);
}
float DCRemover::push(float in_sample)
{
    float oldest_sample;
    
    windowf_index(buff, 0, &oldest_sample);
    windowf_push(buff, in_sample);
    avg += (in_sample - oldest_sample) / (float)len;
    
    return in_sample - avg;
}

StdDev::StdDev(unsigned int len):
len(len),
std_dev(0),
buff(windowf_create(len))
{
}
StdDev::~StdDev(void)
{
    windowf_destroy(buff);
}
float StdDev::push(float in_sample)
{
    float oldest_sample;
    
    windowf_index(buff, 0, &oldest_sample);
    windowf_push(buff, in_sample);
    std_dev = sqrt((std_dev * std_dev * ((float)len - 1.0f) + in_sample * in_sample - oldest_sample * oldest_sample) / (float)(len - 1.0f));
    
    return std_dev;
}

Downconverter::Downconverter(unsigned int in_sample_rate, unsigned int decim_factor, unsigned int carrier_freq, unsigned int bandwidth):
status(DOWNCONV_DEFAULT),
n(0),
decim_factor(decim_factor),
conv_sample(0.0f),
mix_osc(nco_crcf_create(LIQUID_NCO))
{
    float f0_hat = (float)carrier_freq / (float)in_sample_rate;
    float f_cutoff_hat = (float)bandwidth / (float)in_sample_rate / 2.0f;
    float trans_width_hat = (float)TRANS_WIDTH / (float)in_sample_rate;
    unsigned int filt_len = estimate_req_filter_len(trans_width_hat, STOPBAND_ATTEN);
    float *coeffs = new float[filt_len];
    std::complex<float> filt_unscaled_resp;
    
    nco_crcf_set_phase(mix_osc, 0.0f);
    nco_crcf_set_frequency(mix_osc, 2.0f * M_PI * f0_hat);
    
    liquid_firdes_kaiser(filt_len, f_cutoff_hat, STOPBAND_ATTEN, 0.0f, coeffs);
    filt = firfilt_crcf_create(coeffs, filt_len);
    firfilt_crcf_freqresponse(filt, 0, &filt_unscaled_resp);
    firfilt_crcf_set_scale(filt, 1.0f / std::abs(filt_unscaled_resp));
    
    delete [] coeffs;
}
Downconverter::~Downconverter(void)
{
    nco_crcf_destroy(mix_osc);
    firfilt_crcf_destroy(filt);
}
void Downconverter::push(float in_sample)
{
    std::complex<float> in_sample_complex = in_sample;
    std::complex<float> mixed;
    
    nco_crcf_mix_down(mix_osc, in_sample_complex, &mixed);
    nco_crcf_step(mix_osc);
    
    firfilt_crcf_push(filt, mixed);
    
    status = DOWNCONV_DEFAULT;
    if(n % decim_factor == 0)
    {
        firfilt_crcf_execute(filt, &conv_sample);
        status = NEW_CONV_SAMPLE;
    }
    
    n++;
}
downconverter_status Downconverter::getStatus(void)
{
    return status;
}
std::complex<float> Downconverter::getSample(void)
{
    return conv_sample;
}

FSKSynchronizer::FSKSynchronizer(unsigned int sample_rate, const int symbols[2], unsigned int sym_rate, const float *sym_corrections, const float code[], const float orth_code[], unsigned int code_len):
status(SYNCH_DEFAULT),
triggered(0),
n(0),
sample_rate(sample_rate),
sym_rate(sym_rate),
sym_corrections(sym_corrections),
code_len(code_len),
corr_sig_peak(0),
corr_in_buff(windowf_create(BUFF_LEN)),
corr_sig_output_buff(windowf_create(BUFF_LEN)),
corr_orth_output_buff(windowf_create(BUFF_LEN)),
dyn_thresh_buff(windowf_create(BUFF_LEN)),
thresh_calc(THRESH_CALC_LEN)
{
    float space_freq_hat = (float)symbols[0] / (float)sample_rate;
    float mark_freq_hat = (float)symbols[1] / (float)sample_rate;
    float f_cutoff_hat = (float)SYM_WIDTH / (float)sample_rate / 2.0f;
    float trans_width_hat = (float)TRANS_WIDTH / (float)sample_rate;
    unsigned int filt_len = estimate_req_filter_len(trans_width_hat, STOPBAND_ATTEN);
    float *base_coeffs = new float[filt_len];
    std::complex<float> *space_coeffs = new std::complex<float>[filt_len];
    std::complex<float> *mark_coeffs = new std::complex<float>[filt_len];
    std::complex<float> space_unscaled_resp, mark_unscaled_resp;
    float *corr_sig_coeffs = new float[code_len * SAMPLES_PER_SYMBOL];
    float *corr_orth_coeffs = new float[code_len * SAMPLES_PER_SYMBOL];
    
    liquid_firdes_kaiser(filt_len, f_cutoff_hat, STOPBAND_ATTEN, 0.0f, base_coeffs);
    for(unsigned int coeff_no = 0; coeff_no < filt_len; coeff_no++)
    {
        space_coeffs[coeff_no] = base_coeffs[coeff_no] * std::exp(j * (std::complex<float>)(2 * M_PI * (float)(space_freq_hat * (coeff_no + 1))));
        mark_coeffs[coeff_no] = base_coeffs[coeff_no] * std::exp(j * (std::complex<float>)(2 * M_PI * (float)(mark_freq_hat * (coeff_no + 1))));
    }
    space_filt = firfilt_cccf_create(space_coeffs, filt_len);
    mark_filt = firfilt_cccf_create(mark_coeffs, filt_len);
    firfilt_cccf_freqresponse(space_filt, space_freq_hat, &space_unscaled_resp);
    firfilt_cccf_freqresponse(mark_filt, mark_freq_hat, &mark_unscaled_resp);
    firfilt_cccf_set_scale(space_filt, (std::complex<float>)1.0f / space_unscaled_resp);
    firfilt_cccf_set_scale(mark_filt, (std::complex<float>)1.0f / mark_unscaled_resp);

    for(unsigned int coeff_no = 0; coeff_no < code_len * SAMPLES_PER_SYMBOL; coeff_no++)
    {
        corr_sig_coeffs[coeff_no] = code[code_len - 1 - coeff_no / SAMPLES_PER_SYMBOL];
        corr_orth_coeffs[coeff_no] = orth_code[code_len - 1 - coeff_no / SAMPLES_PER_SYMBOL];
    }
    corr_sig = firfilt_rrrf_create(corr_sig_coeffs, code_len * SAMPLES_PER_SYMBOL);
    corr_orth = firfilt_rrrf_create(corr_orth_coeffs, code_len * SAMPLES_PER_SYMBOL);
    
    delete [] base_coeffs;
    delete [] space_coeffs;
    delete [] mark_coeffs;
    delete [] corr_sig_coeffs;
    delete [] corr_orth_coeffs;
}
FSKSynchronizer::~FSKSynchronizer(void)
{
    windowf_destroy(corr_in_buff);
    windowf_destroy(corr_sig_output_buff);
    windowf_destroy(corr_orth_output_buff);
    windowf_destroy(dyn_thresh_buff);
    firfilt_cccf_destroy(space_filt);
    firfilt_cccf_destroy(mark_filt);
    firfilt_rrrf_destroy(corr_sig);
    firfilt_rrrf_destroy(corr_orth);
}
void FSKSynchronizer::push(std::complex<float> in_sample)
{
    firfilt_cccf_push(space_filt, in_sample);
    firfilt_cccf_push(mark_filt, in_sample);
    
    status = SYNCH_DEFAULT;
    if(n % (sample_rate / (sym_rate * SAMPLES_PER_SYMBOL)) == 0)
    {
        std::complex<float> space_ch_sample, mark_ch_sample;
        float corr_sig_output, corr_orth_output;
        
        firfilt_cccf_execute(space_filt, &space_ch_sample);
        firfilt_cccf_execute(mark_filt, &mark_ch_sample);
        
        float corr_in = sym_corrections[1] * std::abs(mark_ch_sample) - sym_corrections[0] * std::abs(space_ch_sample);
        windowf_push(corr_in_buff, corr_in);
        
        firfilt_rrrf_push(corr_sig, corr_in);
        firfilt_rrrf_push(corr_orth, corr_in);
        
        firfilt_rrrf_execute(corr_sig, &corr_sig_output);
        firfilt_rrrf_execute(corr_orth, &corr_orth_output);
        
        windowf_push(corr_sig_output_buff, corr_sig_output);
        windowf_push(corr_orth_output_buff, corr_orth_output);
        
        float dyn_thresh = THRESH_FACTOR * thresh_calc.push(corr_orth_output);
        windowf_push(dyn_thresh_buff, dyn_thresh);
    
        if(triggered)
        {
            if(n - trigger_n > code_len * sample_rate / sym_rate)
            {
                status = SYNCH_LOCKED_ON;
            }
            else if(corr_sig_output > corr_sig_peak)
            {
                corr_sig_peak = corr_sig_output;
                status = NEW_SYNCH_POINT;
            }
        }
        else if(corr_sig_output > dyn_thresh)
        {
            corr_sig_peak = corr_sig_output;
            trigger_n = n;
            triggered = 1;
        }
    }
    
    n++;
}
synchronizer_status FSKSynchronizer::getStatus(void)
{
    return status;
}
windowf FSKSynchronizer::dumpCorrInBuff(void)
{
    return corr_in_buff;
}
windowf FSKSynchronizer::dumpCorrSigOutputBuff(void)
{
    return corr_sig_output_buff;
}
windowf FSKSynchronizer::dumpCorrOrthOutputBuff(void)
{
    return corr_orth_output_buff;
}
windowf FSKSynchronizer::dumpDynThreshBuff(void)
{
    return dyn_thresh_buff;
}
void FSKSynchronizer::rst(void)
{
    triggered = 0;
    status = SYNCH_DEFAULT;
}

FSKDecider::FSKDecider(unsigned int sample_rate, unsigned int bits_per_sym, const int symbols[], const float sym_corrections[], unsigned int sym_rate):
status(DECID_DEFAULT),
n(0),
num_accum_samples(0),
sample_rate(sample_rate),
sym_rate(sym_rate),
num_sym(1 << bits_per_sym),
sym_corrections(sym_corrections),
energies(new float[num_sym]),
highest_energy_sym(0),
filters(new firfilt_cccf[num_sym])
{
    float f_cutoff_hat = (float)SYM_WIDTH / (float)sample_rate / 2.0f;
    float trans_width_hat = (float)TRANS_WIDTH / (float)sample_rate;
    unsigned int filt_len = estimate_req_filter_len(trans_width_hat, STOPBAND_ATTEN);
    float *base_coeffs = new float[filt_len];
    std::complex<float> *rotated_coeffs = new std::complex<float>[filt_len];
    std::complex<float> unscaled_resp;
    
    liquid_firdes_kaiser(filt_len, f_cutoff_hat, STOPBAND_ATTEN, 0.0f, base_coeffs);
    
    for(unsigned int sym_index = 0; sym_index < num_sym; sym_index++)
    {
        float sym_freq_hat = (float)symbols[sym_index] / (float)sample_rate;
        
        for(unsigned int coeff_no = 0; coeff_no < filt_len; coeff_no++)
        {
            rotated_coeffs[coeff_no] = base_coeffs[coeff_no] * std::exp(j * (std::complex<float>)(2 * M_PI * (float)(sym_freq_hat * (coeff_no + 1))));
            filters[sym_index] = firfilt_cccf_create(rotated_coeffs, filt_len);
            firfilt_cccf_freqresponse(filters[sym_index], sym_freq_hat, &unscaled_resp);
            firfilt_cccf_set_scale(filters[sym_index], (std::complex<float>)1.0f / unscaled_resp);
        }
    }
    
    std::memset(energies, 0.0f, num_sym * sizeof(float));
    
    delete [] base_coeffs;
    delete [] rotated_coeffs;
}
FSKDecider::~FSKDecider(void)
{
    delete [] energies;
    
    for(unsigned int sym_index = 0; sym_index < num_sym; sym_index++)
    {
        firfilt_cccf_destroy(filters[sym_index]);
    }
    delete [] filters;
}
void FSKDecider::push(std::complex<float> in_sample)
{
    for(unsigned int sym_index = 0; sym_index < num_sym; sym_index++)
    {
        firfilt_cccf_push(filters[sym_index], in_sample);
    }
    
    if(n % (sample_rate / (sym_rate * SAMPLES_PER_SYMBOL)) == 0)
    {
        std::complex<float> sym_ch_sample;
        
        for(unsigned int sym_index = 0; sym_index < num_sym; sym_index++)
        {
            firfilt_cccf_execute(filters[sym_index], &sym_ch_sample);
            
            energies[sym_index] += sym_corrections[sym_index] * std::abs(sym_ch_sample);
        }
        
        num_accum_samples++;
    }
    
    status = DECID_DEFAULT;
    if(num_accum_samples == SAMPLES_PER_SYMBOL)
    {
        float highest_energy = 0;
        
        for(unsigned int sym_index = 0; sym_index < num_sym; sym_index++)
        {
            if(energies[sym_index] > highest_energy)
            {
                highest_energy = energies[sym_index];
                highest_energy_sym = sym_index;
            }
        }
        num_accum_samples = 0;
        std::memset(energies, 0.0f, num_sym * sizeof(float));
        status = NEW_SYMBOL;
    }
    
    n++;
}
decider_status FSKDecider::getStatus(void)
{
    return status;
}
unsigned int FSKDecider::getSym(void)
{
    return highest_energy_sym;
}
void FSKDecider::rst(void)
{
    num_accum_samples = 0;
    std::memset(energies, 0.0f, num_sym * sizeof(float));
    status = DECID_DEFAULT;
}

SymPacker::SymPacker(unsigned int pkt_size, unsigned int bits_per_sym):
status(PACKER_DEFAULT),
sym_num(0),
bits_per_sym(bits_per_sym),
pkt_size(pkt_size),
sym_buff(new unsigned char[pkt_size * sizeof(unsigned char) / bits_per_sym]),
pkt(new unsigned char[pkt_size])
{
    
}
SymPacker::~SymPacker(void)
{
    delete [] sym_buff;
    delete [] pkt;
}
void SymPacker::push(unsigned int sym)
{
    unsigned int num_written;
    
    sym_buff[sym_num] = sym;
    sym_num++;
    
    status = PACKER_DEFAULT;
    if(sym_num == pkt_size * 8 / bits_per_sym)
    {
        liquid_repack_bytes(sym_buff, bits_per_sym, pkt_size * 8 / bits_per_sym, pkt, 8, pkt_size, &num_written);
        
        sym_num = 0;
        status = PKT_FULL;
    }
}
packer_status SymPacker::getStatus(void)
{
    return status;
}
unsigned char *SymPacker::getPkt(void)
{
    return pkt;
}
void SymPacker::rst(void)
{
    sym_num = 0;
    status = PACKER_DEFAULT;
}
