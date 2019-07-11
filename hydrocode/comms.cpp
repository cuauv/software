//
//  comms.cpp
//  hydromathd
//
//  Created by Vlad on 2/5/19.
//  Copyright © 2019 Vlad. All rights reserved.
//

#include <cstdio>
#include <cstdint>

#include "libshm/c/vars.h"
//#include "shm_mac.hpp"
#include "liquid.h"
#include "comms.hpp"
#include "common_dsp.hpp"
#include "udp_sender.hpp"
#include "structs.hpp"

#include <complex>

static float f0_hat = (float)carrier_freq / (float)sampling_rate;
static float f_cutoff_hat = (float)lowpass_width / (float)sampling_rate / 2.0;
static float lowpass_transition_width_hat = (float)lowpass_transition_width / (float)sampling_rate;
static float space_freq_hat = (float)space_freq / (float)dsp_sampling_rate;
static float mark_freq_hat = (float)mark_freq / (float)dsp_sampling_rate;
static float symbol_f_cutoff_hat = (float)symbol_width / (float)dsp_sampling_rate / 2.0;
static float symbol_transition_width_hat = (float)symbol_transition_width / (float)dsp_sampling_rate;
static unsigned int dsp_decimation_factor = sampling_rate / dsp_sampling_rate;
static unsigned int corr_decimation_factor = dsp_sampling_rate / (bit_rate * corr_resolution);
static unsigned int dc_averaging_length = (long long)dc_averaging_duration * (long long)sampling_rate / 1'000'000;
static unsigned int noise_std_dev_length = (long long)noise_std_dev_duration * (long long)(bit_rate * corr_resolution) / 1'000'000;
static unsigned int min_interpacket_length = (comms_packet_length + gold_code_length) * corr_resolution;
static unsigned int comms_filtered_plot_period_length = (long long)comms_filtered_plot_period * (long long)dsp_sampling_rate / 1'000'000;
static unsigned int comms_filtered_plot_duration_length = (long long)comms_filtered_plot_duration * (long long)(dsp_sampling_rate) / 1'000'000;
static unsigned int corr_plot_length =(long long)corr_plot_duration * (long long)(bit_rate * corr_resolution) / 1'000'000;

static struct hydrophones_settings shm_settings;

windowf input_buffer = windowf_create(buffer_length);
windowf ac_coupled_buffer = windowf_create(buffer_length);
windowcf mixed_buffer = windowcf_create(buffer_length);
windowcf filtered_buffer = windowcf_create(buffer_length);
windowcf space_ch_buffer = windowcf_create(buffer_length);
windowcf mark_ch_buffer = windowcf_create(buffer_length);
windowf corr_buffer = windowf_create(buffer_length);
windowf corr_signal_output_buffer = windowf_create(buffer_length);
windowf corr_noise_output_buffer = windowf_create(buffer_length);
windowf threshold_buffer = windowf_create(buffer_length);
windowcf comms_filtered_plot = windowcf_create(comms_filtered_plot_duration_length);

nco_crcf mixer_oscillator;
firfilt_rrrf corr_signal_fir, corr_noise_fir;
firfilt_crcf lowpass_fir;
firfilt_cccf space_fir, mark_fir;

energy_detector decider(corr_resolution);

static float lowpass_fir_coeffs[2048];
static float symbol_fir_coeffs[512];
static float corr_signal_coeffs[1024], corr_noise_coeffs[1024];
static std::complex<float> space_fir_coeffs[512];
static std::complex<float> mark_fir_coeffs[512];

static unsigned int n, last_comms_filtered_plot_n, triggered_n, corr_peak_n, last_packet_n;
static unsigned int trigger_state;
static unsigned int symbol_counter;
static float input_peak, corr_peak;
static float dc_offset, noise_std_dev;

void commsInit(void)
{
    unsigned int lowpass_fir_length, symbol_fir_length;
    std::complex<float> lowpass_unscaled_response, space_unscaled_response, mark_unscaled_response, space_unscaled_response_inverse, mark_unscaled_response_inverse;

    printf("\nInitializng Comms...\n\n");
    
    mixer_oscillator = nco_crcf_create(LIQUID_NCO);
    nco_crcf_set_phase(mixer_oscillator, 0.0);
    nco_crcf_set_frequency(mixer_oscillator, 2 * M_PI * f0_hat);
    
    lowpass_fir_length = estimate_req_filter_len(lowpass_transition_width_hat, stopband_attenuation);
    liquid_firdes_kaiser(lowpass_fir_length, f_cutoff_hat, stopband_attenuation, 0.0, lowpass_fir_coeffs);
    lowpass_fir = firfilt_crcf_create(lowpass_fir_coeffs, lowpass_fir_length);
    firfilt_crcf_freqresponse(lowpass_fir, 0.0, (liquid_float_complex *)&lowpass_unscaled_response);
    firfilt_crcf_set_scale(lowpass_fir, 1.0 / std::abs(lowpass_unscaled_response));
    
    symbol_fir_length = estimate_req_filter_len(symbol_transition_width_hat, stopband_attenuation);
    liquid_firdes_kaiser(symbol_fir_length, symbol_f_cutoff_hat, stopband_attenuation, 0.0, symbol_fir_coeffs);
    for(unsigned int coeff_no = 0; coeff_no < symbol_fir_length; coeff_no++)
    {
        space_fir_coeffs[coeff_no] = symbol_fir_coeffs[coeff_no] * std::exp(j * (std::complex<float>)(2 * M_PI * (float)(space_freq_hat * (coeff_no + 1))));
        mark_fir_coeffs[coeff_no] = symbol_fir_coeffs[coeff_no] * std::exp(j * (std::complex<float>)(2 * M_PI * (float)(mark_freq_hat * (coeff_no + 1))));
    }
    space_fir = firfilt_cccf_create((liquid_float_complex *)space_fir_coeffs, symbol_fir_length);
    mark_fir = firfilt_cccf_create((liquid_float_complex *)mark_fir_coeffs, symbol_fir_length);
    firfilt_cccf_freqresponse(space_fir, space_freq_hat, (liquid_float_complex *)&space_unscaled_response);
    firfilt_cccf_freqresponse(mark_fir, mark_freq_hat, (liquid_float_complex *)&mark_unscaled_response);
    space_unscaled_response_inverse = (std::complex<float>)1.0 / space_unscaled_response;
    mark_unscaled_response_inverse = (std::complex<float>)1.0 / mark_unscaled_response;
    firfilt_cccf_set_scale(space_fir, *(liquid_float_complex *)&space_unscaled_response_inverse);
    firfilt_cccf_set_scale(mark_fir, *(liquid_float_complex *)&mark_unscaled_response_inverse);
    
    for(unsigned int coeff_no = 0; coeff_no < gold_code_length * corr_resolution; coeff_no++)
    {
        if(gold_code[gold_code_length - 1 - coeff_no / corr_resolution])
        {
            corr_signal_coeffs[coeff_no] = 1.0;
        }
        else
        {
            corr_signal_coeffs[coeff_no] = -1.0;
        }
        
        if(another_gold_code[gold_code_length - 1 - coeff_no / corr_resolution])
        {
            corr_noise_coeffs[coeff_no] = 1.0;
        }
        else
        {
            corr_noise_coeffs[coeff_no] = -1.0;
        }
    }
    corr_signal_fir = firfilt_rrrf_create(corr_signal_coeffs, gold_code_length * corr_resolution);
    corr_noise_fir = firfilt_rrrf_create(corr_noise_coeffs, gold_code_length * corr_resolution);
    
    commsReset();
}

void commsReset(void)
{
    windowf_reset(input_buffer);
    windowf_reset(ac_coupled_buffer);
    windowcf_reset(mixed_buffer);
    windowcf_reset(filtered_buffer);
    windowcf_reset(space_ch_buffer);
    windowcf_reset(mark_ch_buffer);
    windowf_reset(corr_buffer);
    windowf_reset(corr_signal_output_buffer);
    windowf_reset(corr_noise_output_buffer);
    windowf_reset(threshold_buffer);

    n = 0;
    last_comms_filtered_plot_n = 0;
    triggered_n = 0;
    corr_peak_n = 0;
    last_packet_n = 0;
    input_peak = 0;
    corr_peak = 0;
    dc_offset = 0;
    noise_std_dev = 0;
    trigger_state = 0;
    symbol_counter = 0;
    
    decider.reset();
    
    setGain(shm_settings.manual_gain_value);
}

static void copyComplexBuff(windowcf *source, unsigned int source_len, windowcf *target, unsigned int target_len)
{
    std::complex<float> sample;
    
    for(unsigned int sample_no = 0; sample_no < target_len; sample_no++)
    {
        windowcf_index(*source, source_len - 1 - target_len + sample_no, (liquid_float_complex *)&sample);
        windowcf_push(*target, *(liquid_float_complex *)&sample);
    }
}

static void copyRealBuff(windowf *source, unsigned int source_len, windowf *target, unsigned int target_len)
{
    float sample;
    
    for(unsigned int sample_no = 0; sample_no < target_len; sample_no++)
    {
        windowf_index(*source, source_len - 1 - target_len + sample_no, &sample);
        windowf_push(*target, sample);
    }
}

static void getReals(windowcf *source, float *target, unsigned int len)
{
    std::complex<float> sample;
    
    for(unsigned int sample_no = 0; sample_no < len; sample_no++)
    {
        windowcf_index(*source, sample_no, (liquid_float_complex *)&sample);
        target[sample_no] = std::real(sample);
    }
}

static void getImags(windowcf *source, float *target, unsigned int len)
{
    std::complex<float> sample;
    
    for(unsigned int sample_no = 0; sample_no < len; sample_no++)
    {
        windowcf_index(*source, sample_no, (liquid_float_complex *)&sample);
        target[sample_no] = std::imag(sample);
    }
}

static void runningAverage(float *avg, windowf *buff, unsigned int buff_len, unsigned int len)
{
    float sample_to_add, sample_to_remove;
    
    windowf_index(*buff, buff_len - 1, &sample_to_add);
    windowf_index(*buff, buff_len - 1 - len, &sample_to_remove);
    
    *avg += (sample_to_add - sample_to_remove) / (float)len;
}

static void runningStdDev(float *std_dev, windowf *buff, unsigned int buff_len, unsigned int len)
{
    float sample_to_add, sample_to_remove;
    
    windowf_index(*buff, buff_len - 1, &sample_to_add);
    windowf_index(*buff, buff_len - 1 - len, &sample_to_remove);
    
    *std_dev = sqrt((*std_dev * *std_dev * ((float)len - 1.0) + sample_to_add * sample_to_add - sample_to_remove * sample_to_remove) / (float)(len - 1));
}

void commsDSP(uint16_t *fpga_packet, unsigned int packet_no)
{
    for(unsigned int packet_sample_no = 0; packet_sample_no < packet_length; packet_sample_no++)
    {
        float input_sample, ac_coupled_sample, corr_sample, corr_signal_output, corr_noise_output;
        std::complex<float> mixed_sample, filtered_sample, space_ch_sample, mark_ch_sample;
        bool new_symbol;
        
        input_sample = fpga_packet[4 * packet_sample_no + 3];
        windowf_push(input_buffer, input_sample);
        
        if(input_sample > abs(input_peak))
        {
            input_peak = input_sample;
            
            copyComplexBuff(&filtered_buffer, buffer_length, &comms_filtered_plot, comms_filtered_plot_duration_length);
        }
        
        runningAverage(&dc_offset, &input_buffer, buffer_length, dc_averaging_length);
        ac_coupled_sample = input_sample - dc_offset;
        windowf_push(ac_coupled_buffer, ac_coupled_sample);
    
        nco_crcf_mix_down(mixer_oscillator, *(liquid_float_complex *)&ac_coupled_sample, reinterpret_cast<liquid_float_complex *>(&mixed_sample));
        windowcf_push(mixed_buffer, *(liquid_float_complex *)&mixed_sample);
        
        firfilt_crcf_push(lowpass_fir, *(liquid_float_complex *)&mixed_sample);
        if(n % dsp_decimation_factor == 0)
        {
            firfilt_crcf_execute(lowpass_fir, (liquid_float_complex *)&filtered_sample);
            windowcf_push(filtered_buffer, *(liquid_float_complex *)&filtered_sample);
            
            firfilt_cccf_push(space_fir, *(liquid_float_complex *)&filtered_sample);
            firfilt_cccf_push(mark_fir, *(liquid_float_complex *)&filtered_sample);
            
            if((n / dsp_decimation_factor) % corr_decimation_factor == 0)
            {
                firfilt_cccf_execute(space_fir, (liquid_float_complex *)&space_ch_sample);
                windowcf_push(space_ch_buffer, *(liquid_float_complex *)&space_ch_sample);
                
                firfilt_cccf_execute(mark_fir, (liquid_float_complex *)&mark_ch_sample);
                windowcf_push(mark_ch_buffer, *(liquid_float_complex *)&mark_ch_sample);
                
                corr_sample = mark_correction_factor * std::abs(mark_ch_sample) - space_correction_factor * std::abs(space_ch_sample);
                windowf_push(corr_buffer, corr_sample);
                
                firfilt_rrrf_push(corr_signal_fir, corr_sample);
                firfilt_rrrf_execute(corr_signal_fir, &corr_signal_output);
                windowf_push(corr_signal_output_buffer, corr_signal_output);
                
                firfilt_rrrf_push(corr_noise_fir, corr_sample);
                firfilt_rrrf_execute(corr_noise_fir, &corr_noise_output);
                windowf_push(corr_noise_output_buffer, corr_noise_output);
                
                runningStdDev(&noise_std_dev, &corr_noise_output_buffer, buffer_length, noise_std_dev_length);
                windowf_push(threshold_buffer, threshold_factor * noise_std_dev);
                
                switch(trigger_state)
                {
                    case 0:
                    {
                        if(corr_signal_output > threshold_factor * noise_std_dev && (n - last_packet_n) / (dsp_decimation_factor * corr_decimation_factor) > min_interpacket_length)
                        {
                            printf("trigg\n");
                            printf("%d\n", (n - last_packet_n) / (dsp_decimation_factor * corr_decimation_factor) - min_interpacket_length);
                            triggered_n = n;
                            trigger_state = 1;
                        }
                        break;
                    }
                    case 1:
                    {
                        if(corr_signal_output > corr_peak)
                        {
                            corr_peak = corr_signal_output;
                            corr_peak_n = n;
                        }
                        if((n - triggered_n) / (dsp_decimation_factor * corr_decimation_factor) > corr_resolution * gold_code_length)
                        {
                            last_packet_n = corr_peak_n;
                            
                            if((n - last_packet_n) / (dsp_decimation_factor * corr_decimation_factor) > corr_plot_length / 2)
                            {
                                float *plot_ptr;
                                
                                windowf_read(corr_buffer, &plot_ptr);
                                sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                                windowf_read(corr_signal_output_buffer, &plot_ptr);
                                sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                                windowf_read(corr_noise_output_buffer, &plot_ptr);
                                sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                                windowf_read(threshold_buffer, &plot_ptr);
                                sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                                
                                //printf("corr plot sent\n");
                            }
                            
                            for(unsigned int past_sample_no = (n - corr_peak_n) / (dsp_decimation_factor * corr_decimation_factor); past_sample_no > 0; past_sample_no--)
                            {
                                std::complex <float> space_sample, mark_sample;
                                
                                windowcf_index(space_ch_buffer, buffer_length - 1 - past_sample_no, (liquid_float_complex *)&space_sample);
                                windowcf_index(mark_ch_buffer, buffer_length - 1 - past_sample_no, (liquid_float_complex *)&mark_sample);
                                
                                new_symbol = decider.push(space_sample, mark_sample);
                                if(new_symbol)
                                {
                                    printf("%d", decider.getSymbol());
                                    
                                    symbol_counter++;
                                    if(symbol_counter == comms_packet_length)
                                    {
                                        break;
                                    }
                                }
                            }
                            //printf("done with past samples");
                            
                            if(symbol_counter == comms_packet_length)
                            {
                                printf("\n");
                                symbol_counter = 0;
                                trigger_state = 0;
                            }
                            else
                            {
                                trigger_state = 2;
                            }
                        }
                        break;
                    }
                    case 2:
                    {
                        new_symbol = decider.push(space_ch_sample, mark_ch_sample);
                        if(new_symbol)
                        {
                            printf("%d", decider.getSymbol());
                         
                            symbol_counter++;
                            if(symbol_counter == comms_packet_length)
                            {
                                symbol_counter = 0;
                                trigger_state = 0;
                                printf("\n");
                            }
                        }
                        break;
                    }
                    default:
                    {
                        break;
                    }
                }
                
                if((n - last_packet_n) / (dsp_decimation_factor * corr_decimation_factor) == corr_plot_length / 2)
                {
                    float *plot_ptr;
                    
                    windowf_read(corr_buffer, &plot_ptr);
                    sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                    windowf_read(corr_signal_output_buffer, &plot_ptr);
                    sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                    windowf_read(corr_noise_output_buffer, &plot_ptr);
                    sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                    windowf_read(threshold_buffer, &plot_ptr);
                    sendPlot(plot_ptr + buffer_length - 1 - corr_plot_length, 4, corr_plot_length);
                    
                    //printf("corr plot sent\n");
                }
            }
            
            if((n - last_comms_filtered_plot_n) / dsp_decimation_factor == comms_filtered_plot_period_length)
            {
                float plot[comms_filtered_plot_duration_length];
                getReals(&comms_filtered_plot, plot, comms_filtered_plot_duration_length);
                sendPlot(plot, 3, comms_filtered_plot_duration_length);
                getImags(&comms_filtered_plot, plot, comms_filtered_plot_duration_length);
                sendPlot(plot, 3, comms_filtered_plot_duration_length);
                
                //printf("comms filtered plot sent\n");
                
                input_peak = 0;
                last_comms_filtered_plot_n = n;
            }
        }
        
        nco_crcf_step(mixer_oscillator);
        n++;
    }
}
