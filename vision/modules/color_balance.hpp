#pragma once
#include <iostream>
#include <cmath>
#include <thread>
#include <opencv/cv.h>
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>

extern "C" {
    int process_frame(unsigned char *arr, size_t height, size_t width, size_t depth,
            bool equalize_rgb, bool rgb_contrast_correct, bool hsv_contrast_correct,
            bool hsi_contrast_correct, bool rgb_extrema_clipping, bool adaptive_cast_correction,
            int horizontal_blocks, int vertical_blocks);
}
