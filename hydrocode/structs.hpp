//
//  structs.hpp
//  hydromathd
//
//  Since most operations must be performed on all three channels at the same time, it makes sense to group values and buffers into single structs.
//
//  Created by Vlad on 9/29/18.
//  Copyright © 2018 Vlad. All rights reserved.
//

#ifndef structs_hpp
#define structs_hpp

#include "liquid.h"
#include "comms.hpp"

#include <complex>

struct triple_sample
{
    //Makes a triple sample.
    
    float ch0, ch1, ch2;
    
    triple_sample()
    {
        ch0 = 0;
        ch1 = 0;
        ch2 = 0;
    }
    
    float max(void)
    {
        //Returns the highest sample in the group.
        
        float max;

        max = ch0;

        if(ch1 > max)
        {
            max = ch1;
        }
        if(ch2 > max)
        {
            max = ch2;
        }

        return max;
    }
    
    triple_sample operator +(const triple_sample &other_sample)
    {
        //Adds the samples from two groups.
        
        triple_sample result;
        
        result.ch0 = ch0 + other_sample.ch0;
        result.ch1 = ch1 + other_sample.ch1;
        result.ch2 = ch2 + other_sample.ch2;
        
        return result;
    }
    
    triple_sample operator -(const triple_sample &other_sample)
    {
        //Subtracts the samples from two groups.
        
        triple_sample result;
        
        result.ch0 = ch0 - other_sample.ch0;
        result.ch1 = ch1 - other_sample.ch1;
        result.ch2 = ch2 - other_sample.ch2;
        
        return result;
    }
    
    triple_sample operator *(const float &factor)
    {
        //Multiplies the samples by a constant.
        
        triple_sample result;
        
        result.ch0 = ch0 * factor;
        result.ch1 = ch1 * factor;
        result.ch2 = ch2 * factor;
        
        return result;
    }
    
    triple_sample operator /(const float &divisor)
    {
        //Divides the samples by a constant.
        
        triple_sample result;
        
        result.ch0 = ch0 / divisor;
        result.ch1 = ch1 / divisor;
        result.ch2 = ch2 / divisor;
        
        return result;
    }
};

struct triple_phasor
{
    //Makes a triple phasor.
    
    std::complex<float> ch0, ch1, ch2;
    
    triple_phasor(void)
    {
        ch0 = 0;
        ch1 = 0;
        ch2 = 0;
    }
    
    triple_sample phase(void)
    {
        //Returns the phases of the phasors.
        
        triple_sample phasor_phase;
        
        phasor_phase.ch0 = std::arg(ch0);
        phasor_phase.ch1 = std::arg(ch1);
        phasor_phase.ch2 = std::arg(ch2);
        
        return phasor_phase;
    }
    
    float combinedAmplitude(void)
    {
        //Returns the sum of the phasors's amplitudes.
        
        return std::norm(ch0) + std::norm(ch1) + std::norm(ch2);
    }
    
    void operator =(const triple_sample &other_sample)
    {
        //Assigns a value to the three phasors.
        
        ch0 = other_sample.ch0;
        ch1 = other_sample.ch1;
        ch2 = other_sample.ch2;
    }
   
    triple_phasor operator +(const triple_phasor &other_phasor)
    {
        //Adds the phasors from two groups.
        
        triple_phasor result;
        
        result.ch0 = ch0 + other_phasor.ch0;
        result.ch1 = ch1 + other_phasor.ch1;
        result.ch2 = ch2 + other_phasor.ch2;
        
        return result;
    }
    
    triple_phasor operator -(const triple_phasor &other_phasor)
    {
        //Subtracts the phasors from two groups.
        
        triple_phasor result;
        
        result.ch0 = ch0 - other_phasor.ch0;
        result.ch1 = ch1 - other_phasor.ch1;
        result.ch2 = ch2 - other_phasor.ch2;
        
        return result;
    }
    
    triple_phasor operator *(const std::complex<float> &factor)
    {
        //Multiplies the phasors by a constant.
        
        triple_phasor result;
        
        result.ch0 = ch0 * factor;
        result.ch1 = ch1 * factor;
        result.ch2 = ch2 * factor;
        
        return result;
    }
};

struct buffer_triple
{
    //Makes a triple windowf buffer.
    
    windowf ch0, ch1, ch2;
    
    buffer_triple(const int &buffer_length)
    {
        ch0 = windowf_create(buffer_length);
        ch1 = windowf_create(buffer_length);
        ch2 = windowf_create(buffer_length);
    }
    
    triple_sample read(const int &sample_index) const
    {
        //Returns the three sample group at index "sample_index".
        
        triple_sample read_sample;
        
        windowf_index(ch0, sample_index, &read_sample.ch0);
        windowf_index(ch1, sample_index, &read_sample.ch1);
        windowf_index(ch2, sample_index, &read_sample.ch2);
        
        return read_sample;
    }
    
    float *get(const int &channel) const
    {
        //Returns the data pointer for one of the buffers.
        
        float *channel_array;
        
        if(channel == 0) windowf_read(ch0, &channel_array);
        else if(channel == 1) windowf_read(ch1, &channel_array);
        else windowf_read(ch2, &channel_array);
        
        return channel_array;
    }
    
    void push(const triple_sample &sample_to_push)
    {
        //Pushes a three sample group.
        
        windowf_push(ch0, sample_to_push.ch0);
        windowf_push(ch1, sample_to_push.ch1);
        windowf_push(ch2, sample_to_push.ch2);
    }
  
    void clear()
    {
        //Fills buffer with zeros
        
        windowf_reset(ch0);
        windowf_reset(ch1);
        windowf_reset(ch2);
    }
};

struct buffer
{
    //Just a wrapper to a single windowf buffer.
    
    windowf value;
    
    buffer(const int &buffer_length)
    {
        value = windowf_create(buffer_length);
    }
    
    float read(const int &value_index) const
    {
        //Returns the sample at "value_index".
        
        float read_value;
        
        windowf_index(value, value_index, &read_value);
        
        return read_value;
    }
    
    float *get() const
    {
        //Returns the data pointer for the buffer.
        
        float *values_array;
        
        windowf_read(value, &values_array);
        
        return values_array;
    }
    
    void push(const float &value_to_push)
    {
        //Pushes a single sample.
        
        windowf_push(value, value_to_push);
    }
    
    void clear()
    {
        //Fills buffer with zeros
        
        windowf_reset(value);
    }
};

struct energy_detector
{
    float space_energy, mark_energy;
    bool symbol;
    unsigned int samples_per_symbol;
    unsigned int sample_counter;
    
    energy_detector(unsigned int corr_resolution)
    {
        space_energy = 0;
        mark_energy = 0;
        samples_per_symbol = corr_resolution;
        sample_counter = 0;
    }
    
    void reset(void)
    {
        space_energy = 0;
        mark_energy = 0;
        sample_counter = 0;
        symbol = 0;
    }
    
    bool push(std::complex <float> space_sample, std::complex <float> mark_sample)
    {
        space_energy += space_correction_factor * std::abs(space_sample);
        mark_energy += mark_correction_factor * std::abs(mark_sample);
        
        sample_counter++;
        if(sample_counter == samples_per_symbol)
        {
            if(space_energy > mark_energy)
            {
                symbol = 0;
            }
            else
            {
                symbol = 1;
            }
            
            space_energy = 0;
            mark_energy = 0;
            sample_counter = 0;
            
            return true;
        }
        else
        {
            return false;
        }
    }
    
    bool getSymbol(void)
    {
        return symbol;
    }
};

#endif /* structs_hpp */
