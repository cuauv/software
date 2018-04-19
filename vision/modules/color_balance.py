#!/usr/bin/env python3

import cv2
import shm
import numpy
import time
import ctypes
from auv_python_helpers import load_library
from vision.modules.base import ModuleBase
from vision import options

options = [ options.BoolOption('verbose', False) ]
_lib_color_balance = load_library('libauv-color-balance.so')

"""
Convert from RGB color space to HSI color space
"""
def conv_rgb_to_hsi(r_channel, g_channel, b_channel):
    #start_time = time.time()
    i_channel = (r_channel + g_channel + b_channel) / 3
    rgb_min_channel = numpy.minimum(r_channel, g_channel)
    rgb_min_channel = numpy.minimum(rgb_min_channel, b_channel)
    s_channel = 1 - (rgb_min_channel / i_channel)
    g_gte_b_channel = g_channel >= b_channel
    h_channel1 = numpy.arccos((r_channel - (g_channel / 2) - (b_channel / 2)) /
                                           (numpy.sqrt(r_channel**2 +
                                               g_channel**2 + b_channel**2 -
                                               (r_channel * g_channel) -
                                               (r_channel * b_channel) -
                                               (g_channel * b_channel))))
    h_channel2 = (2 * numpy.pi) - h_channel1
    h_channel1[g_gte_b_channel == False] = 0
    h_channel2[g_gte_b_channel] = 0
    h_channel = h_channel1 + h_channel2
    h_channel[numpy.isnan(h_channel)] = 0
    s_channel[numpy.isnan(s_channel)] = 0
    h_channel = numpy.clip(h_channel, 0, 2 * numpy.pi)
    s_channel = numpy.clip(s_channel, 0.0, 1.0)
    i_channel = numpy.clip(i_channel, 0.0, 255.0)
    #print("rgb->hsi elapsed: " + str(time.time() - start_time))
    return (h_channel, s_channel, i_channel)

"""
Convert from HSI color space to RGB color space
"""
def conv_hsi_to_rgb(h_channel, s_channel, i_channel):
    #start_time = time.time()
    i_times_s = i_channel * s_channel
    cos_60_minus_h = numpy.cos(numpy.deg2rad(60) - h_channel)
    cos_h_minus_120 = numpy.cos(h_channel - numpy.deg2rad(120))
    cos_180_minus_h = numpy.cos(numpy.deg2rad(180) - h_channel)
    cos_h_minus_240 = numpy.cos(h_channel - numpy.deg2rad(240))
    cos_300_minus_h = numpy.cos(numpy.deg2rad(300) - h_channel)
    cos_h_minus_240_div_cos_300_minus_h = cos_h_minus_240 / cos_300_minus_h
    cos_h_div_cos_60_minus_h = numpy.cos(h_channel) / cos_60_minus_h
    cos_h_minus_120_div_cos_180_minus_h = cos_h_minus_120 / cos_180_minus_h
    i_times_s_times_cos_h_div_cos_60_minus_h = i_times_s * cos_h_div_cos_60_minus_h
    i_times_s_times_cos_h_minus_240_div_cos_300_minus_h = i_times_s * cos_h_minus_240_div_cos_300_minus_h
    i_times_s_times_cos_h_minus_120_div_cos_180_minus_h = i_times_s * cos_h_minus_120_div_cos_180_minus_h
    r_channel1 = i_channel + i_times_s_times_cos_h_div_cos_60_minus_h
    r_channel2 = i_channel - i_times_s
    r_channel3 = i_channel + (i_times_s - i_times_s_times_cos_h_minus_240_div_cos_300_minus_h)
    g_channel1 = i_channel + (i_times_s - i_times_s_times_cos_h_div_cos_60_minus_h)
    g_channel2 = i_channel + (i_times_s * cos_h_minus_120_div_cos_180_minus_h)
    g_channel3 = r_channel2.copy()
    b_channel1 = r_channel2.copy()
    b_channel2 = i_channel + (i_times_s - i_times_s_times_cos_h_minus_120_div_cos_180_minus_h)
    b_channel3 = i_channel + (i_times_s * cos_h_minus_240_div_cos_300_minus_h)
    r_channel1[h_channel >= numpy.deg2rad(120)] = 0
    g_channel1[h_channel >= numpy.deg2rad(120)] = 0
    b_channel1[h_channel >= numpy.deg2rad(120)] = 0
    r_channel2[h_channel < numpy.deg2rad(120)] = 0
    g_channel2[h_channel < numpy.deg2rad(120)] = 0
    b_channel2[h_channel < numpy.deg2rad(120)] = 0
    r_channel2[h_channel >= numpy.deg2rad(240)] = 0
    g_channel2[h_channel >= numpy.deg2rad(240)] = 0
    b_channel2[h_channel >= numpy.deg2rad(240)] = 0
    r_channel3[h_channel < numpy.deg2rad(240)] = 0
    g_channel3[h_channel < numpy.deg2rad(240)] = 0
    b_channel3[h_channel < numpy.deg2rad(240)] = 0
    r_channel = r_channel1 + r_channel2 + r_channel3
    g_channel = g_channel1 + g_channel2 + g_channel3
    b_channel = b_channel1 + b_channel2 + b_channel3
    r_channel = numpy.clip(r_channel, 0., 255.);
    g_channel = numpy.clip(g_channel, 0., 255.);
    b_channel = numpy.clip(b_channel, 0., 255.);
    #print("hsi->rgb elapsed: " + str(time.time() - start_time))
    return (r_channel, g_channel, b_channel)

def test_hsi_conversion(r, g, b):
    return conv_hsi_to_rgb(*conv_rgb_to_hsi(numpy.array([r], dtype=numpy.float64),
                                            numpy.array([g], dtype=numpy.float64),
                                            numpy.array([b], dtype=numpy.float64)))
def balance(mat, equalize_rgb=True, rgb_contrast_correct=False,
            hsv_contrast_correct=True, hsi_contrast_correct=False,
            rgb_extrema_clipping=True, adaptive_cast_correction=True,
            horizontal_blocks=1, vertical_blocks=1):
    rows = mat.shape[0]
    cols = mat.shape[1]
    depth = 3

    # Convert to one-dimensional uint8 array and pass into C++ module
    c_uint8_p = ctypes.POINTER(ctypes.c_int8)
    data = mat.flatten()
    data_p = data.ctypes.data_as(c_uint8_p)
    _lib_color_balance.process_frame(data_p, rows, cols, depth, equalize_rgb,
            rgb_contrast_correct, hsv_contrast_correct, hsi_contrast_correct,
            rgb_extrema_clipping, adaptive_cast_correction, horizontal_blocks, vertical_blocks)
    # Convert to matrix of original shape
    mat = numpy.ctypeslib.as_array(data_p, (rows, cols, depth)).astype(numpy.uint8)
    return mat

class ColorBalance(ModuleBase):
    def process(self, mat):
        start_time = time.time()
        self.post('orig', mat)

        mat = balance(mat)
        self.post('balanced', mat)

        #RGB_CONTRAST_CORRECT = False # Faster, but tends to overcompensate,
        #                             # resulting in opposite color cast
        #HLS_CONTRAST_CORRECT = False # Faster, but colors can get washed out if
        #                             # saturation or lightness is too high, due
        #                             # to correspondence between saturation and
        #                             # lightness
        #HSV_CONTRAST_CORRECT = False # Faster, but shifts color cast, due to
        #                             # calculation of value
        #HSI_CONTRAST_CORRECT = True  # Best quality, but slower
        #RGB_EXTREMA_CLIPPING = True
        #self.post('original', mat)
        #bgr_split = cv2.split(mat)

        #b_channel = bgr_split[0]
        #g_channel = bgr_split[1]
        #r_channel = bgr_split[2]

        #if RGB_EXTREMA_CLIPPING:
        #    # Clip 0.4% of outliers (0.2% from top and bottom)
        #    r_min_max = numpy.percentile(r_channel, [0.2, 99.8])
        #    g_min_max = numpy.percentile(g_channel, [0.2, 99.8])
        #    b_min_max = numpy.percentile(b_channel, [0.2, 99.8])

        #    r_channel = numpy.clip(r_channel, r_min_max[0], r_min_max[1])
        #    g_channel = numpy.clip(g_channel, g_min_max[0], g_min_max[1])
        #    b_channel = numpy.clip(b_channel, b_min_max[0], b_min_max[1])
        #    r_min = r_min_max[0]
        #    r_max = r_min_max[1]
        #    g_min = g_min_max[0]
        #    g_max = g_min_max[1]
        #    b_min = b_min_max[0]
        #    b_max = b_min_max[1]
        #else:
        #    r_min = numpy.amin(r_channel)
        #    g_min = numpy.amin(g_channel)
        #    b_min = numpy.amin(b_channel)
        #    r_max = numpy.amax(r_channel)
        #    g_max = numpy.amax(g_channel)
        #    b_max = numpy.amax(b_channel)

        #r_avg = numpy.mean(r_channel)
        #g_avg = numpy.mean(g_channel)
        #b_avg = numpy.mean(b_channel)

        #rgb_split_float = [r_channel.astype(numpy.float64),
        #                   g_channel.astype(numpy.float64),
        #                   b_channel.astype(numpy.float64)]
        #r_channel = rgb_split_float[0]
        #g_channel = rgb_split_float[1]
        #b_channel = rgb_split_float[2]

        ## Equalize RGB
        #print("Original avgs:\t\t", r_avg, g_avg, b_avg)
        ## Red color cast
        #if r_avg > g_avg and r_avg > b_avg:
        #    g_gain = r_avg / g_avg
        #    b_gain = r_avg / b_avg
        #    g_channel *= g_gain
        #    b_channel *= b_gain
        ## Green color cast
        #elif g_avg > r_avg and g_avg > b_avg:
        #    r_gain = g_avg / r_avg
        #    b_gain = g_avg / b_avg
        #    r_channel *= r_gain
        #    b_channel *= b_gain
        ## Blue color cast
        #else:
        #    r_gain = b_avg / r_avg
        #    g_gain = b_avg / g_avg
        #    r_channel *= r_gain
        #    g_channel *= g_gain

        #if RGB_CONTRAST_CORRECT:
        #    # RGB Contrast Correction
        #    r_channel = numpy.clip(r_channel, 0., 255.)
        #    g_channel = numpy.clip(g_channel, 0., 255.)
        #    b_channel = numpy.clip(b_channel, 0., 255.)
        #    avgs = [{'avg': r_avg, 'min': r_min, 'max': r_max, 'channel': r_channel},
        #            {'avg': g_avg, 'min': g_min, 'max': g_max, 'channel': g_channel},
        #            {'avg': b_avg, 'min': b_min, 'max': b_max, 'channel': b_channel}]
        #    avgs = sorted(avgs, key=lambda x: x['avg'])

        #    desired_max = (avgs[0]['max'] + avgs[1]['max'] + avgs[2]['max']) / 3
        #    # Contrast correct towards upper side
        #    avgs[0]['channel'] -= avgs[0]['min']
        #    avgs[0]['channel'] *= (desired_max - avgs[0]['min']) / (avgs[0]['max'] - avgs[0]['min'])
        #    # Contrast correct towards both sides
        #    avgs[1]['channel'] -= avgs[1]['min']
        #    avgs[1]['channel'] *= (desired_max - 0) / (avgs[1]['max'] - avgs[1]['min'])
        #    # Contrast correct towards lower side
        #    avgs[2]['channel'] -= avgs[2]['min']
        #    avgs[2]['channel'] *= (avgs[2]['max'] - 0) / (avgs[2]['max'] - avgs[2]['min'])
        #    r_channel = numpy.clip(r_channel, 0., 255.)
        #    g_channel = numpy.clip(g_channel, 0., 255.)
        #    b_channel = numpy.clip(b_channel, 0., 255.)

        #if HLS_CONTRAST_CORRECT:
        #    # HLS Contrast Correction
        #    r_channel = numpy.clip(r_channel, 0., 255.)
        #    g_channel = numpy.clip(g_channel, 0., 255.)
        #    b_channel = numpy.clip(b_channel, 0., 255.)
        #    rgb_split = [r_channel.astype(numpy.uint8),
        #                 g_channel.astype(numpy.uint8),
        #                 b_channel.astype(numpy.uint8)]
        #    balanced_mat = cv2.merge(rgb_split)
        #    [h_channel, l_channel, s_channel] = cv2.split(cv2.cvtColor(balanced_mat, cv2.COLOR_RGB2HLS))
        #    h_channel = h_channel.astype(numpy.float64)
        #    l_channel = l_channel.astype(numpy.float64)
        #    s_channel = s_channel.astype(numpy.float64)
        #    # Clip Saturation and Lightness outliers
        #    s_min_max = numpy.percentile(s_channel, [0.2, 99.8])
        #    s_channel = numpy.clip(s_channel, s_min_max[0], s_min_max[1])
        #    l_min_max = numpy.percentile(l_channel, [0.2, 99.8])
        #    l_channel = numpy.clip(l_channel, l_min_max[0], l_min_max[1])
        #    s_min = s_min_max[0]
        #    s_max = s_min_max[1]
        #    s_channel = (s_channel - s_min) * ((255. - 0.) / (s_max - s_min))
        #    l_min = l_min_max[0]
        #    l_max = l_min_max[1]
        #    print(s_min_max, l_min_max)
        #    l_channel = (l_channel - l_min) * ((255. - 0.) / (l_max - l_min))
        #    hls_split = [h_channel.astype(numpy.uint8),
        #                 l_channel.astype(numpy.uint8),
        #                 s_channel.astype(numpy.uint8)]
        #    balanced_mat = cv2.merge(hls_split)
        #    balanced_mat = cv2.cvtColor(balanced_mat, cv2.COLOR_HLS2RGB)
        #    [r_channel, g_channel, b_channel] = cv2.split(balanced_mat)

        #if HSV_CONTRAST_CORRECT:
        #    # HSV Contrast Correction
        #    r_channel = numpy.clip(r_channel, 0., 255.)
        #    g_channel = numpy.clip(g_channel, 0., 255.)
        #    b_channel = numpy.clip(b_channel, 0., 255.)
        #    rgb_split = [r_channel.astype(numpy.uint8),
        #                 g_channel.astype(numpy.uint8),
        #                 b_channel.astype(numpy.uint8)]
        #    balanced_mat = cv2.merge(rgb_split)
        #    [h_channel, s_channel, v_channel] = cv2.split(cv2.cvtColor(balanced_mat, cv2.COLOR_RGB2HSV))
        #    h_channel = h_channel.astype(numpy.float64)
        #    s_channel = s_channel.astype(numpy.float64)
        #    v_channel = v_channel.astype(numpy.float64)
        #    # Clip Saturation and Lightness outliers
        #    s_min_max = numpy.percentile(s_channel, [0.2, 99.8])
        #    s_channel = numpy.clip(s_channel, s_min_max[0], s_min_max[1])
        #    v_min_max = numpy.percentile(v_channel, [0.2, 99.8])
        #    v_channel = numpy.clip(v_channel, v_min_max[0], v_min_max[1])
        #    s_min = s_min_max[0]
        #    s_max = s_min_max[1]
        #    s_channel = (s_channel - s_min) * ((255. - 0.) / (s_max - s_min))
        #    v_min = v_min_max[0]
        #    v_max = v_min_max[1]
        #    print(s_min_max, v_min_max)
        #    v_channel = (v_channel - v_min) * ((255. - 0.) / (v_max - v_min))
        #    hsv_split = [h_channel.astype(numpy.uint8),
        #                 s_channel.astype(numpy.uint8),
        #                 v_channel.astype(numpy.uint8)]
        #    balanced_mat = cv2.merge(hsv_split)
        #    balanced_mat = cv2.cvtColor(balanced_mat, cv2.COLOR_HSV2RGB)
        #    [r_channel, g_channel, b_channel] = cv2.split(balanced_mat)

        #if HSI_CONTRAST_CORRECT:
        #    # HSI Contrast Correction
        #    r_channel = numpy.clip(r_channel, 0., 255.)
        #    g_channel = numpy.clip(g_channel, 0., 255.)
        #    b_channel = numpy.clip(b_channel, 0., 255.)
        #    # TODO clip for better results
        #    h_channel, s_channel, i_channel = conv_rgb_to_hsi(r_channel, g_channel, b_channel)
        #    s_min_max = numpy.percentile(s_channel, [0.2, 99.8])
        #    s_channel = numpy.clip(s_channel, s_min_max[0], s_min_max[1])
        #    i_min_max = numpy.percentile(i_channel, [0.2, 99.8])
        #    i_channel = numpy.clip(i_channel, i_min_max[0], i_min_max[1])
        #    s_min = s_min_max[0]
        #    s_max = s_min_max[1]
        #    #s_min = numpy.amin(s_channel)
        #    #s_max = numpy.amax(s_channel)
        #    s_channel = (s_channel - s_min) * (1. / (s_max - s_min))
        #    i_min = i_min_max[0]
        #    i_max = i_min_max[1]
        #    #i_min = numpy.amin(i_channel)
        #    #i_max = numpy.amax(i_channel)
        #    i_channel = (i_channel - i_min) * (255. / (i_max - i_min))
        #    r_channel, g_channel, b_channel = conv_hsi_to_rgb(h_channel, s_channel, i_channel)

        #print("Contrast corrected avgs:\t", numpy.mean(r_channel), numpy.mean(g_channel), numpy.mean(b_channel))

        ## Convert back to int array
        #bgr_split = [b_channel.astype(numpy.uint8),
        #             g_channel.astype(numpy.uint8),
        #             r_channel.astype(numpy.uint8)]
        #balanced_mat = cv2.merge(bgr_split)
        #self.post('balanced', balanced_mat)
        end_time = time.time()
        print("Elapsed time: " + str(end_time - start_time))

if __name__ == '__main__':
    ColorBalance('forward', options)()
