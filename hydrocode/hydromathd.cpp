//
//  hydromathd.cpp
//  hydromathd
//
//  Created by Vlad on 9/10/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#include <cstdint>
#include <complex>

#include "liquid.h"
#include "libshm/c/vars.h"
//#include "shm_mac.hpp"
#include "pinger_tracking.hpp"
#include "comms.hpp"
#include "udp_receiver.hpp"
#include "udp_sender.hpp"
#include "constants.hpp"

static void printPkt(unsigned char *pkt, unsigned int size)
{
    for(unsigned int word_no = 0; word_no < size; word_no++)
    {
        printf("0x%X ", pkt[word_no]);
    }
    printf("\n");
}

void saveBuff(float *target, unsigned int target_len, windowf src, unsigned int src_len)
{
    float *buff_ptr;
    
    windowf_read(src, &buff_ptr);
    
    memcpy(target, buff_ptr + src_len - target_len, target_len * sizeof(float));
}

void saveComplexBuff(float *target, unsigned int target_len, windowcf src, unsigned int src_len)
{
    std::complex<float> *buff_ptr;
    
    windowcf_read(src, reinterpret_cast<liquid_float_complex **>(&buff_ptr));
    
    memcpy(target, buff_ptr + src_len - target_len, target_len * sizeof(std::complex<float>));
}

int main()
{
    uint16_t sample_pkt[4 * SAMPLE_PKT_LEN];
    unsigned int n = 0, last_comms_baseb_plot_n = 0;
    float baseb_peak = 0;
    float comms_baseb_plot[2 * Comms::BASEB_PLOT_LEN];
    float comms_corr_in_plot[Comms::CORR_PLOT_LEN];
    float comms_corr_sig_output_plot[Comms::CORR_PLOT_LEN];
    float comms_corr_orth_output_plot[Comms::CORR_PLOT_LEN];
    float comms_dyn_thresh_plot[Comms::CORR_PLOT_LEN];
    
	struct hydrophones_settings shm_settings;
	struct hydrophones_status shm_status;
    
    windowcf baseb_buff = windowcf_create(BUFF_LEN);
    
    UDPSampleReceiver sample_recvr(SAMPLE_PORT);
    DCRemover comms_dc_remov(DC_AVG_PER);
    Downconverter comms_downconv(ADC_SAMPLE_RATE, Comms::DSP_DECIM_FACTOR, Comms::CARRIER_FREQ, Comms::BANDWIDTH);
    FSKSynchronizer comms_synch(ADC_SAMPLE_RATE / Comms::DSP_DECIM_FACTOR, Comms::SYNCH_SYMBOLS, Comms::SYM_RATE, Comms::SYNCH_SYM_CORRECTIONS, Comms::CODE, Comms::ORTH_CODE, Comms::CODE_LEN);
    FSKDecider comms_decid(ADC_SAMPLE_RATE / Comms::DSP_DECIM_FACTOR, Comms::BITS_PER_SYM, Comms::SYMBOLS, Comms::SYM_CORRECTIONS, Comms::SYM_RATE);
    SymPacker comms_pack(Comms::PKT_SIZE, Comms::BITS_PER_SYM);
    UDPPlotSender comms_baseb_plot_sender(Comms::BASEB_PLOT_ADDR, Comms::BASEB_PLOT_PORT);
    UDPPlotSender comms_corr_plot_sender(Comms::CORR_PLOT_ADDR, Comms::CORR_PLOT_PORT);

    shm_init();
    
    shm_status.packet_count = 0; 

    while(1)
    {
        shm_getg(hydrophones_settings, shm_settings);
        
        sample_recvr.recv(sample_pkt, SAMPLE_PKT_LEN);
        
        if(shm_settings.choose_one_for_tracking_choose_zero_for_communications == 1)
        {
            pingerTracking(sample_pkt);
        }
        else
        {
            for(unsigned int sample_no = 0; sample_no < SAMPLE_PKT_LEN; sample_no++)
            {
                float in_sample = sample_pkt[sample_no * 4 + 3];
                
                float ac_sample = comms_dc_remov.push(in_sample);
                
                comms_downconv.push(ac_sample);
                if(comms_downconv.getStatus() == NEW_CONV_SAMPLE)
                {
                    std::complex<float> baseb_sample = comms_downconv.getSample();
                    comms_synch.push(baseb_sample);
                    
                    if(comms_synch.getStatus() == NEW_SYNCH_POINT)
                    {
                        comms_decid.rst();
                        comms_pack.rst();
                        
                        saveBuff(comms_corr_in_plot, Comms::CORR_PLOT_LEN, comms_synch.dumpCorrInBuff(), BUFF_LEN);
                        saveBuff(comms_corr_sig_output_plot, Comms::CORR_PLOT_LEN, comms_synch.dumpCorrSigOutputBuff(), BUFF_LEN);
                        saveBuff(comms_corr_orth_output_plot, Comms::CORR_PLOT_LEN, comms_synch.dumpCorrOrthOutputBuff(), BUFF_LEN);
                        saveBuff(comms_dyn_thresh_plot, Comms::CORR_PLOT_LEN, comms_synch.dumpDynThreshBuff(), BUFF_LEN);
                    }
                    
                    comms_decid.push(baseb_sample);
                    if(comms_decid.getStatus() == NEW_SYMBOL && comms_pack.getStatus() != PKT_FULL)
                    {
                        unsigned int sym = comms_decid.getSym();
                        comms_pack.push(sym);
                    }

                    if(comms_synch.getStatus() == SYNCH_LOCKED_ON && comms_pack.getStatus() == PKT_FULL)
                    {
                        comms_synch.rst();
                        
                        unsigned char *pkt = comms_pack.getPkt();
                        printPkt(pkt, Comms::PKT_SIZE);
                        
                        comms_corr_plot_sender.send(comms_corr_in_plot, Comms::CORR_PLOT_LEN);
                        comms_corr_plot_sender.send(comms_corr_sig_output_plot, Comms::CORR_PLOT_LEN);
                        comms_corr_plot_sender.send(comms_corr_orth_output_plot, Comms::CORR_PLOT_LEN);
                        comms_corr_plot_sender.send(comms_dyn_thresh_plot, Comms::CORR_PLOT_LEN);
                    }
                    
                    windowcf_push(baseb_buff, baseb_sample);
                    if(std::abs(baseb_sample) > baseb_peak)
                    {
                        baseb_peak = std::abs(baseb_sample);
                        saveComplexBuff(comms_baseb_plot, Comms::BASEB_PLOT_LEN, baseb_buff, BUFF_LEN);
                    }
                }
                
                if(n - last_comms_baseb_plot_n == Comms::BASEB_PLOT_PER)
                {
                    comms_baseb_plot_sender.send(comms_baseb_plot, 2 * Comms::BASEB_PLOT_LEN);
                    baseb_peak = 0;
                    last_comms_baseb_plot_n = n;
                }
            
                n++;
            }
        }

        shm_status.packet_count++;
        shm_setg(hydrophones_status, shm_status);
    }
    
    //toodles!!!!!
    return 0;
}
