#include "misc/utils.h"
#include "color_balance.hpp"

#define CACHE_INVALID (-1)
static double EPSILON = 0.000001;
float ****rgb_to_hsi_cache = NULL;
bool cache_rgb_to_hsi = true;

bool feq(float a, float b) {
    return fabs(a - b) < EPSILON;
}

unsigned char constrain(double val, unsigned char low, unsigned char high) {
    if (val < low) {
        return low;
    }
    else if (val > high) {
        return high;
    }
    else {
        return (unsigned char)val;
    }
}

void clip_channel_helper(unsigned char *channel,
                         size_t start,
                         size_t end,
                         unsigned char min,
                         unsigned char max) {
    for (size_t i = start; i < end; ++i) {
        if (channel[i] < min) {
            channel[i] = min;
        }
        else if (channel[i] > max) {
            channel[i] = max;
        }
    }
}

void clip_channel(unsigned char *channel, size_t channel_size, unsigned char min, unsigned char max) {
    std::thread worker1(clip_channel_helper, channel, 0, channel_size / 2, min, max);
    std::thread worker2(clip_channel_helper, channel, channel_size / 2, channel_size, min, max);
    worker1.join();
    worker2.join();
}

void clip_channel_f_helper(float *channel, size_t start, size_t end, float min, float max) {
    for (size_t i = start; i < end; ++i) {
        if (channel[i] < min) {
            channel[i] = min;
        }
        else if (channel[i] > max) {
            channel[i] = max;
        }
        else if (std::isnan(channel[i]) ){
            channel[i] = min;
        }
        else if (std::isinf(channel[i])) {
            channel[i] = max;
        }
    }
}

void clip_channel_f(float *channel, size_t channel_size, float min, float max) {
    std::thread worker1(clip_channel_f_helper, channel, 0, channel_size / 2, min, max);
    std::thread worker2(clip_channel_f_helper, channel, channel_size / 2, channel_size, min, max);
    worker1.join();
    worker2.join();
}

int partition (float *a, int left, int right, int pivotIdx) {
    float pivot = a[pivotIdx];
    float tmp;
    tmp = a[right];
    a[right] = a[pivotIdx];
    a[pivotIdx] = tmp;
    int wall = left;
    for (int i = left; i < right; ++i) {
        if (a[i] < pivot) {
            tmp = a[wall];
            a[wall] = a[i];
            a[i] = tmp;
            ++wall;
        }
    }
    tmp = a[wall];
    a[wall] = a[right];
    a[right] = tmp;
    return wall;
}

float quickselect(float *a, int left, int right, int n) {
    for (int i = left; i < right; ++i) {
        if (left == right) {
            return a[left];
        }
        int randIdx = (std::rand() % (right - left)) + left;
        int pivotIdx = partition(a, left, right, randIdx);
        if (n == pivotIdx) {
            return a[n];
        }
        else if (n < pivotIdx) {
            right = pivotIdx - 1;
        }
        else {
            left = pivotIdx + 1;
        }
    }
    return 0;
}

unsigned char *percentile_min_max(unsigned char *channel, size_t channel_size, float lower_perc, float upper_perc) {
    int low_bound = (int) (lower_perc * (float) (channel_size));
    int high_bound = channel_size - (int) (upper_perc * (float) (channel_size));
    int counts[256];
    for (int i = 0; i < 256; ++i) {
        counts[i] = 0;
    }
    for (size_t i = 0; i < channel_size; ++i) {
        ++counts[(int)channel[i]];
    }
    unsigned char *min_max = (unsigned char *) malloc(sizeof(unsigned char) * 2);
    for (int i = 0; i < 256; ++i) {
        if (low_bound < counts[i]) {
            min_max[0] = i;
            break;
        }
        else {
            low_bound -= counts[i];
        }
    }
    for (int i = 255; i >= 0; --i) {
        if (high_bound < counts[i]) {
            min_max[1] = i;
            break;
        }
        else {
            high_bound -= counts[i];
        }
    }
    return min_max;
}

float *percentile_min_max_qselect(float *channel, size_t channel_size, float lower_perc, float upper_perc) {
    int low_bound = (int) (lower_perc * (float) (channel_size));
    int high_bound = (int) (upper_perc * (float) (channel_size));
    float *channel_copy = (float *) malloc(sizeof(*channel_copy) * channel_size);
    memcpy(channel_copy, channel, sizeof(float) * channel_size);
    float *min_max = (float *) malloc(sizeof(*min_max) * 2);
    min_max[0] = quickselect(channel_copy, 0, channel_size - 1, low_bound);
    min_max[1] = quickselect(channel_copy, min_max[0], channel_size - 1, high_bound);
    free(channel_copy);
    return min_max;
}

unsigned char uchar_clip(float f, unsigned char min, unsigned char max) {
    int n = (int)f;
    if (n < min) {
        n = min;
    }
    else if (n > max) {
        n = max;
    }
    return n;
}

void conv_rgb_to_hsi_helper(unsigned char *r_channel,
                            unsigned char *g_channel,
                            unsigned char *b_channel,
                            float **hsi,
                            size_t start,
                            size_t end) {
    unsigned char r, g, b;
    unsigned char min_of_rgb;
    for (size_t i = start; i < end; ++i) {
        min_of_rgb = 255;
        r = r_channel[i];
        g = g_channel[i];
        b = b_channel[i];
        if (cache_rgb_to_hsi && rgb_to_hsi_cache[r][g][b][0] != CACHE_INVALID) {
            hsi[0][i] = rgb_to_hsi_cache[r][g][b][0];
            hsi[1][i] = rgb_to_hsi_cache[r][g][b][1];
            hsi[2][i] = rgb_to_hsi_cache[r][g][b][2];
            continue;
        }
        hsi[2][i] = ((float)r + (float)g + (float)b) / 3.;
        if (r < min_of_rgb) {
            min_of_rgb = r;
        }
        if (g < min_of_rgb) {
            min_of_rgb = g;
        }
        if (b < min_of_rgb) {
            min_of_rgb = b;
        }
        if (hsi[2][i] > 0) {
            hsi[1][i] = 1. - ((float)min_of_rgb / hsi[2][i]);
        }
        else {
            hsi[1][i] = 0;
        }
        hsi[0][i] = acos(((float)r - (0.5 * g) - (0.5 * b)) /
                        sqrt((float)r*r + (float)g*g + (float)b*b -
                            (float)(r*g) - (float)(r*b) - (float)(g*b)));
        if (b > g) {
            hsi[0][i] = (M_PI * 2) - hsi[0][i];
        }

        if (cache_rgb_to_hsi) {
            rgb_to_hsi_cache[r][g][b][0] = hsi[0][i];
            rgb_to_hsi_cache[r][g][b][1] = hsi[1][i];
            rgb_to_hsi_cache[r][g][b][2] = hsi[2][i];
        }
        //printf("%f, %f, %f\n", hsi[0][i], hsi[1][i], hsi[2][i]);
    }
}

float **conv_rgb_to_hsi(unsigned char *r_channel,
                        unsigned char *g_channel,
                        unsigned char *b_channel,
                        size_t channel_size) {
    float **hsi = (float **) malloc(sizeof(float *) * 3);
    for (int i = 0; i < 3; ++i) {
        hsi[i] = (float *) malloc(sizeof(float) * channel_size);
    }
    if (cache_rgb_to_hsi && rgb_to_hsi_cache == NULL) {
        rgb_to_hsi_cache = (float ****)malloc(sizeof(float ***) * 256);
        for (int r = 0; r < 256; ++r) {
            rgb_to_hsi_cache[r] = (float ***)malloc(sizeof(float **) * 256);
            for (int g = 0; g < 256; ++g) {
                rgb_to_hsi_cache[r][g] = (float **)malloc(sizeof(float *) * 256);
                for (int b = 0; b < 256; ++b) {
                    rgb_to_hsi_cache[r][g][b] = (float *)malloc(sizeof(float) * 3);
                    for (int channel = 0; channel < 3; ++channel) {
                        rgb_to_hsi_cache[r][g][b][channel] = CACHE_INVALID;
                    }
                }
            }
        }
    }
    std::thread worker1(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, 0, channel_size / 8);
    std::thread worker2(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, channel_size / 8, channel_size / 4);
    std::thread worker3(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, channel_size / 4, 3 * channel_size / 8);
    std::thread worker4(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, 3 * channel_size / 8, channel_size / 2);
    std::thread worker5(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, channel_size / 2, 5 * channel_size / 8);
    std::thread worker6(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, 5 * channel_size / 8, 3 * channel_size / 4);
    std::thread worker7(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, 3 * channel_size / 4, 7 * channel_size / 8);
    std::thread worker8(conv_rgb_to_hsi_helper, r_channel, g_channel, b_channel, hsi, 7 * channel_size / 8, channel_size);
    worker1.join();
    worker2.join();
    worker3.join();
    worker4.join();
    worker5.join();
    worker6.join();
    worker7.join();
    worker8.join();
    clip_channel_f(hsi[0], channel_size, 0., 2. * M_PI);
    clip_channel_f(hsi[1], channel_size, 0., 1.);
    clip_channel_f(hsi[2], channel_size, 0., 255.);
    return hsi;
}

void conv_hsi_to_rgb_helper(float *h_channel,
                            float *s_channel,
                            float *i_channel,
                            unsigned char **rgb,
                            size_t start,
                            size_t end) {
    float h, s, i;
    for (size_t j = start; j < end; ++j) {
        h = h_channel[j];
        s = s_channel[j];
        i = i_channel[j];
        //h = 0;
        //s = 0;
        //i = 0;
        if (feq(h, 0)) {
            rgb[0][j] = uchar_clip(i + 2 * i * s, 0, 255);
            rgb[1][j] = uchar_clip(i - i * s, 0, 255);
            rgb[2][j] = uchar_clip(i - i * s, 0, 255);
        }
        else {
            if (0. < h && h < 2. * M_PI / 3.) {
                rgb[0][j] = uchar_clip(i + i * s * cos(h) / cos (M_PI / 3. - h), 0, 255);
                rgb[1][j] = uchar_clip(i + i * s * (1 - cos(h) / cos (M_PI / 3. - h)), 0, 255);
                rgb[2][j] = uchar_clip(i - i * s, 0, 255);
            }
            else if (feq(h, 2. * M_PI / 3.)) {
                rgb[0][j] = uchar_clip(i - i * s, 0, 255);
                rgb[1][j] = uchar_clip(i + 2 * i * s, 0, 255);
                rgb[2][j] = uchar_clip(i - i * s, 0, 255);
            }
            else if (2. * M_PI / 3. < h && h < 4. * M_PI / 3.) {
                rgb[0][j] = uchar_clip(i - i * s, 0, 255);
                rgb[1][j] = uchar_clip(i + i * s * cos(h - 2. * M_PI / 3.) / cos(M_PI - h), 0, 255);
                rgb[2][j] = uchar_clip(i + i * s * (1 - cos(h - 2. * M_PI / 3.) / cos(M_PI - h)), 0, 255);
            }
            else if (feq(h, 4. * M_PI / 3.)) {
                rgb[0][j] = uchar_clip(i - i * s, 0, 255);
                rgb[1][j] = uchar_clip(i - i * s, 0, 255);
                rgb[2][j] = uchar_clip(i + 2 * i * s, 0, 255);
            }
            else {
                rgb[0][j] = uchar_clip(i + i * s * (1 - cos(h - 4. * M_PI / 3.) / cos(5. * M_PI / 3. - h)), 0, 255);
                rgb[1][j] = uchar_clip(i - i * s, 0, 255);
                rgb[2][j] = uchar_clip(i + i * s * cos(h - 4. * M_PI / 3.) / cos(5. * M_PI / 3. - h), 0, 255);
            }
        }
        //printf("%d, %d, %d\n", rgb[0][j], rgb[1][j], rgb[2][j]);
    }
}

unsigned char **conv_hsi_to_rgb(float *h_channel,
                                float *s_channel,
                                float *i_channel,
                                size_t channel_size) {
    unsigned char **rgb = (unsigned char **) malloc(sizeof(unsigned char *) * 3);
    for (int j = 0; j < 3; ++j) {
        rgb[j] = (unsigned char *) malloc(sizeof(unsigned char) * channel_size);
    }
    std::thread worker1(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, 0, channel_size / 8);
    std::thread worker2(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, channel_size / 8, channel_size / 4);
    std::thread worker3(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, channel_size / 4, 3 * channel_size / 8);
    std::thread worker4(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, 3 * channel_size / 8, channel_size / 2);
    std::thread worker5(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, channel_size / 2, 5 * channel_size / 8);
    std::thread worker6(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, 5 * channel_size / 8, 3 * channel_size / 4);
    std::thread worker7(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, 3 * channel_size / 4, 7 * channel_size / 8);
    std::thread worker8(conv_hsi_to_rgb_helper, h_channel, s_channel, i_channel, rgb, 7 * channel_size / 8, channel_size);
    worker1.join();
    worker2.join();
    worker3.join();
    worker4.join();
    worker5.join();
    worker6.join();
    worker7.join();
    worker8.join();
    clip_channel(rgb[0], channel_size, 0, 255);
    clip_channel(rgb[1], channel_size, 0, 255);
    clip_channel(rgb[2], channel_size, 0, 255);
    return rgb;
}

int process_frame(unsigned char *arr, size_t height, size_t width, size_t depth,
                  bool equalize_rgb, bool rgb_contrast_correct,
                  bool hsv_contrast_correct, bool hsi_contrast_correct,
                  bool rgb_extrema_clipping, bool adaptive_cast_correction,
                  int horizontal_blocks, int vertical_blocks) {

    // equalize_rgb             - Removes color cast from image, but may cause washing out if image is entirely one color
    // rgb_contrast_correct     - Faster, but tends to overcompensate, resulting in opposite color cast
    // hsv_contrast_correct     - Faster, but shifts color cast, due to calculation of value
    // hsi_contrast_correct     - Best quality, but slower
    // rgb_extrema_clipping     - Clip outliers in RGB color space
    // adaptive_cast_correction - If false, then the same gain is used for the same component of every pixel
    //                            If true, then the gain decreases as the pixel value nears 255 (max)
    // horizontal_blocks        - Number of horizontal tiles to use when performing localized RGB equalization
    // vertical_blocks          - Number of vertical tiles to use when performing localized RGB equalization

    //printf("equalize_rgb: %d\n", equalize_rgb);
    //printf("rgb_contrast_correct: %d\n", rgb_contrast_correct);
    //printf("hsv_contrast_correct: %d\n", hsv_contrast_correct);
    //printf("hsi_contrast_correct: %d\n", hsi_contrast_correct);
    //printf("rgb_extrema_clipping: %d\n", rgb_extrema_clipping);
    //printf("adaptive_cast_correction: %d\n", adaptive_cast_correction);
    //printf("horizontal_blocks: %d\n", horizontal_blocks);
    //printf("vertical_blocks: %d\n", vertical_blocks);

    cv::Mat mat = cv::Mat(height, width, CV_8UC3, arr);
    size_t channel_size = height * width;
    cv::Mat bgr_split[3];
    cv::split(mat, bgr_split);

    unsigned char *b_channel = bgr_split[0].data;
    unsigned char *g_channel = bgr_split[1].data;
    unsigned char *r_channel = bgr_split[2].data;

    //float **hsi = conv_rgb_to_hsi(r_channel, g_channel, b_channel, channel_size);
    //unsigned char **rgb = conv_hsi_to_rgb(hsi[0], hsi[1], hsi[2], channel_size);
    //for (size_t i = 0; i < channel_size; ++i) {
    //    if (r_channel[i] != rgb[0][i]) {
    //        printf("%d, %d, %d, -> %d, %d, %d\n", r_channel[i], g_channel[i], b_channel[i], rgb[0][i], rgb[1][i], rgb[2][i]);
    //    }
    //}
    //free(hsi[0]);
    //free(hsi[1]);
    //free(hsi[2]);
    //free(hsi);
    //memcpy(r_channel, rgb[0], channel_size * sizeof(unsigned char));
    //memcpy(g_channel, rgb[1], channel_size * sizeof(unsigned char));
    //memcpy(b_channel, rgb[2], channel_size * sizeof(unsigned char));
    //free(rgb[0]);
    //free(rgb[1]);
    //free(rgb[2]);
    //free(rgb);

    double r_min, r_max, g_min, g_max, b_min, b_max;
    double r_avg, g_avg, b_avg;
    if (rgb_extrema_clipping) {
        // Clip 0.4% of outliers (0.2% from top and bottom)
        unsigned char *min_max;

        min_max = percentile_min_max(r_channel, channel_size, 0.002, 0.998);
        r_min = min_max[0];
        r_max = min_max[1];
        clip_channel(r_channel, channel_size, r_min, r_max);
        free(min_max);

        min_max = percentile_min_max(g_channel, channel_size, 0.002, 0.998);
        g_min = min_max[0];
        g_max = min_max[1];
        clip_channel(g_channel, channel_size, g_min, g_max);
        free(min_max);

        min_max = percentile_min_max(b_channel, channel_size, 0.002, 0.998);
        b_min = min_max[0];
        b_max = min_max[1];
        clip_channel(b_channel, channel_size, b_min, b_max);
        free(min_max);
    }
    else {
        cv::minMaxLoc(bgr_split[2], &r_min, &r_max, NULL, NULL);
        cv::minMaxLoc(bgr_split[1], &g_min, &g_max, NULL, NULL);
        cv::minMaxLoc(bgr_split[0], &b_min, &b_max, NULL, NULL);
    }

    r_avg = cv::mean(bgr_split[2]).val[0];
    g_avg = cv::mean(bgr_split[1]).val[0];
    b_avg = cv::mean(bgr_split[0]).val[0];

    //printf("r_min: %lf\n", r_min);
    //printf("r_max: %lf\n", r_max);
    //printf("r_avg: %lf\n", r_avg);
    //printf("g_min: %lf\n", g_min);
    //printf("g_max: %lf\n", g_max);
    //printf("g_avg: %lf\n", g_avg);
    //printf("b_min: %lf\n", b_min);
    //printf("b_max: %lf\n", b_max);
    //printf("b_avg: %lf\n", b_avg);

    // Equalize RGB
    if (equalize_rgb) {
        int block_width = width / horizontal_blocks;
        int block_height = height / vertical_blocks;
        if (width % horizontal_blocks != 0) {
            ++horizontal_blocks;
        }
        if (height % vertical_blocks != 0) {
            ++vertical_blocks;
        }
        for (int block_y = 0; block_y < vertical_blocks; ++block_y) {
            for (int block_x = 0; block_x < horizontal_blocks; ++block_x) {
                //printf("block_x: %d\n", block_x);
                //printf("block_y: %d\n", block_y);
                double local_r_avg = 0;
                double local_g_avg = 0;
                double local_b_avg = 0;
                int count = 0;
                //printf("channel_index: %ld\n", (block_y * block_height) * width + (block_x * block_width));
                for (int j = 0; j < block_height; ++j) {
                    for (int i = 0; i < block_width; ++i) {
                        size_t channel_index = (block_y * block_height + j) * width + (block_x * block_width + i);
                        if (channel_index >= channel_size) {
                            break;
                        }
                        ++count;
                        local_r_avg += (r_channel[channel_index] - local_r_avg) / count;
                        local_g_avg += (g_channel[channel_index] - local_g_avg) / count;
                        local_b_avg += (b_channel[channel_index] - local_b_avg) / count;
                    }
                }
                //printf("local_r_avg: %lf\n", local_r_avg);
                //printf("local_g_avg: %lf\n", local_g_avg);
                //printf("local_b_avg: %lf\n", local_b_avg);
                if (abs(local_r_avg - r_avg) > r_avg / 6 || abs(local_b_avg - b_avg) > b_avg / 6 || abs(local_g_avg - g_avg) > g_avg / 6) {
                    local_r_avg = r_avg;
                    local_b_avg = b_avg;
                    local_g_avg = g_avg;
                }
                // Red color cast
                if (local_r_avg > local_g_avg && local_r_avg > local_b_avg) {
                    double g_gain = local_r_avg / local_g_avg;
                    double b_gain = local_r_avg / local_b_avg;
                    for (int j = 0; j < block_height; ++j) {
                        for (int i = 0; i < block_width; ++i) {
                            size_t channel_index = (block_y * block_height + j) * width + (block_x * block_width + i);
                            if (channel_index >= channel_size) {
                                break;
                            }
                            if (adaptive_cast_correction) {
                                g_channel[channel_index] = constrain(g_channel[channel_index] * (pow((255. - g_channel[channel_index]) / 255., 0.25) * (g_gain - 1.) + 1.), 0, 255);
                                b_channel[channel_index] = constrain(b_channel[channel_index] * (pow((255. - b_channel[channel_index]) / 255., 0.25) * (b_gain - 1.) + 1.), 0, 255);
                            }
                            else {
                                g_channel[channel_index] = constrain(g_channel[channel_index] * g_gain, 0, 255);
                                b_channel[channel_index] = constrain(b_channel[channel_index] * b_gain, 0, 255);
                            }
                        }
                    }
                }
                // Green color cast
                else if (local_g_avg > local_r_avg && local_g_avg > local_b_avg) {
                    double r_gain = local_g_avg / local_r_avg;
                    double b_gain = local_g_avg / local_b_avg;
                    for (int j = 0; j < block_height; ++j) {
                        for (int i = 0; i < block_width; ++i) {
                            size_t channel_index = (block_y * block_height + j) * width + (block_x * block_width + i);
                            if (channel_index >= channel_size) {
                                break;
                            }
                            if (adaptive_cast_correction) {
                                r_channel[channel_index] = constrain(r_channel[channel_index] * (pow((255. - r_channel[channel_index]) / 255., 0.25) * (r_gain - 1.) + 1.), 0, 255);
                                b_channel[channel_index] = constrain(b_channel[channel_index] * (pow((255. - b_channel[channel_index]) / 255., 0.25) * (b_gain - 1.) + 1.), 0, 255);
                            }
                            else {
                                r_channel[channel_index] = constrain(r_channel[channel_index] * r_gain, 0, 255);
                                b_channel[channel_index] = constrain(b_channel[channel_index] * b_gain, 0, 255);
                            }
                        }
                    }
                }
                // Blue color cast
                else {
                    double r_gain = local_b_avg / local_r_avg;
                    double g_gain = local_b_avg / local_g_avg;
                    for (int j = 0; j < block_height; ++j) {
                        for (int i = 0; i < block_width; ++i) {
                            size_t channel_index = (block_y * block_height + j) * width + (block_x * block_width + i);
                            if (channel_index >= channel_size) {
                                break;
                            }
                            if (adaptive_cast_correction) {
                                r_channel[channel_index] = constrain(r_channel[channel_index] * (pow((255. - r_channel[channel_index]) / 255., 0.25) * (r_gain - 1.) + 1.), 0, 255);
                                g_channel[channel_index] = constrain(g_channel[channel_index] * (pow((255. - g_channel[channel_index]) / 255., 0.25) * (g_gain - 1.) + 1.), 0, 255);
                            }
                            else {
                                r_channel[channel_index] = constrain(r_channel[channel_index] * r_gain, 0, 255);
                                g_channel[channel_index] = constrain(g_channel[channel_index] * g_gain, 0, 255);
                            }
                        }
                    }
                }
            }
        }
    }

    if (rgb_contrast_correct) {
        // RGB Contrast Correction
        clip_channel(r_channel, channel_size, 0, 255);
        clip_channel(g_channel, channel_size, 0, 255);
        clip_channel(b_channel, channel_size, 0, 255);
        unsigned char *min_channel;
        unsigned char *mid_channel;
        unsigned char *max_channel;
        int min_channel_min = 0;
        int min_channel_max = 255;
        int mid_channel_min = 0;
        int mid_channel_max = 255;
        int max_channel_min = 0;
        int max_channel_max = 255;

        if (r_avg > g_avg) {
            if (r_avg > b_avg) {
                max_channel = r_channel;
                max_channel_min = r_min;
                max_channel_max = r_max;
                if (g_avg > b_avg) {
                    mid_channel = g_channel;
                    mid_channel_min = g_min;
                    mid_channel_max = g_max;
                    min_channel = b_channel;
                    min_channel_min = b_min;
                    min_channel_max = b_max;
                }
                else {
                    mid_channel = b_channel;
                    mid_channel_min = b_min;
                    mid_channel_max = b_max;
                    min_channel = g_channel;
                    min_channel_min = g_min;
                    max_channel_max = g_max;
                }
            }
            else {
                max_channel = b_channel;
                max_channel_min = b_min;
                max_channel_max = b_max;
                mid_channel = r_channel;
                mid_channel_min = r_min;
                mid_channel_max = r_max;
                min_channel = g_channel;
                min_channel_min = g_min;
                max_channel_max = g_max;
            }
        }
        else { // r_avg <= g_avg
            if (g_avg > b_avg) {
                max_channel = g_channel;
                max_channel_min = g_min;
                max_channel_max = g_max;
                if (r_avg > b_avg) {
                    mid_channel = r_channel;
                    mid_channel_min = r_min;
                    mid_channel_max = r_max;
                    min_channel = b_channel;
                    min_channel_min = b_min;
                    max_channel_max = b_max;
                }
                else {
                    mid_channel = b_channel;
                    mid_channel_min = b_min;
                    mid_channel_max = b_max;
                    min_channel = r_channel;
                    min_channel_min = r_min;
                    max_channel_max = r_max;
                }
            }
            else {
                max_channel = b_channel;
                max_channel_min = b_min;
                max_channel_max = b_max;
                mid_channel = g_channel;
                mid_channel_min = g_min;
                mid_channel_max = g_max;
                min_channel = r_channel;
                min_channel_min = r_min;
                max_channel_max = r_max;
            }
        }

        int desired_max = ((int)min_channel_max + (int)mid_channel_max + (int)max_channel_max) / 3;
        for (size_t i = 0; i < channel_size; ++i) {
            // Contrast correct towards upper side
            min_channel[i] = (unsigned char)(((int)min_channel[i] - min_channel_min) * (desired_max - min_channel_min) /
                                  (min_channel_max - min_channel_min));
            // Contrast correct towards both sides
            mid_channel[i] = (unsigned char)(((int)mid_channel[i] - mid_channel_min) * (desired_max - -1) /
                                  (mid_channel_max - mid_channel_min));
            // Contrast correct towards lower side
            max_channel[i] = (unsigned char)(((int)max_channel[i] - max_channel_min) * (max_channel_max - 0) /
                                  (max_channel_max - max_channel_min));
        }
        clip_channel(r_channel, channel_size, 0, 255);
        clip_channel(g_channel, channel_size, 0, 255);
        clip_channel(b_channel, channel_size, 0, 255);
    }

    if (hsv_contrast_correct) {
        // HSV Contrast Correction
        clip_channel(r_channel, channel_size, 0, 255);
        clip_channel(g_channel, channel_size, 0, 255);
        clip_channel(b_channel, channel_size, 0, 255);
        cv::merge(bgr_split, 3, mat);
        cv::Mat hsv_mat;
        cv::cvtColor(mat, hsv_mat, cv::COLOR_BGR2HSV);
        cv::Mat hsv_split[3];
        cv::split(hsv_mat, hsv_split);

        unsigned char *h_channel = hsv_split[0].data;
        unsigned char *s_channel = hsv_split[1].data;
        unsigned char *v_channel = hsv_split[2].data;

        // Clip Saturation and Lightness outliers
        unsigned char *min_max;
        int s_min, s_max;
        int v_min, v_max;

        min_max = percentile_min_max(s_channel, channel_size, 0.002, 0.998);
        s_min = min_max[0];
        s_max = min_max[1];
        clip_channel(s_channel, channel_size, s_min, s_max);
        free(min_max);

        min_max = percentile_min_max(v_channel, channel_size, 0.002, 0.998);
        v_min = min_max[0];
        v_max = min_max[1];
        clip_channel(v_channel, channel_size, v_min, v_max);
        free(min_max);

        //printf("s_min: %f\n", s_min);
        //printf("s_max: %f\n", s_max);
        //printf("v_min: %f\n", v_min);
        //printf("v_max: %f\n", v_max);
        for (size_t i = 0; i < channel_size; ++i) {
            s_channel[i] = (unsigned char)((((int)s_channel[i] - s_min) * (255 - 0)) / (s_max - s_min));
            v_channel[i] = (unsigned char)((((int)v_channel[i] - v_min) * (255 - 0)) / (v_max - v_min));
        }

        clip_channel(h_channel, channel_size, 0, 255);
        clip_channel(s_channel, channel_size, 0, 255);
        clip_channel(v_channel, channel_size, 0, 255);

        cv::merge(hsv_split, 3, hsv_mat);
        cv::cvtColor(hsv_mat, mat, cv::COLOR_HSV2BGR);

        // Update RGB channels
        cv::split(mat, bgr_split);
        b_channel = bgr_split[0].data;
        g_channel = bgr_split[1].data;
        r_channel = bgr_split[2].data;
    }

    if (hsi_contrast_correct) {
        // HSI Contrast Correction
        clip_channel(r_channel, channel_size, 0, 255);
        clip_channel(g_channel, channel_size, 0, 255);
        clip_channel(b_channel, channel_size, 0, 255);
        float **hsi = conv_rgb_to_hsi(r_channel, g_channel, b_channel, channel_size);
        float *s_channel = hsi[1];
        float *i_channel = hsi[2];

        // Clip saturation and intensity outliers
        float *min_max;
        float s_min, s_max;
        float i_min, i_max;

        min_max = percentile_min_max_qselect(s_channel, channel_size, 0.002, 0.998);
        s_min = min_max[0];
        s_max = min_max[1];
        clip_channel_f(s_channel, channel_size, s_min, s_max);
        free(min_max);

        min_max = percentile_min_max_qselect(i_channel, channel_size, 0.002, 0.998);
        i_min = min_max[0];
        i_max = min_max[1];
        clip_channel_f(i_channel, channel_size, i_min, i_max);
        free(min_max);

        //float s_min = 1.0, s_max = 0.0;
        //float i_min = 255., i_max = 0.;
        //for (size_t i = 0; i < channel_size; ++i) {
        //    if (s_channel[i] < s_min) {
        //        s_min = s_channel[i];
        //    }
        //    else if (s_channel[i] > s_max) {
        //        s_max = s_channel[i];
        //    }
        //    if (i_channel[i] < i_min) {
        //        i_min = i_channel[i];
        //    }
        //    else if (i_channel[i] > i_max) {
        //        i_max = i_channel[i];
        //    }
        //}

        //printf("s_min: %f\n", s_min);
        //printf("s_max: %f\n", s_max);
        //printf("i_min: %f\n", i_min);
        //printf("i_max: %f\n", i_max);

        float s_mult = 1. / (s_max - s_min);
        float i_mult = 255. / (i_max - i_min);
        for (size_t i = 0; i < channel_size; ++i) {
            s_channel[i] = (s_channel[i] - s_min) * s_mult;
            i_channel[i] = (i_channel[i] - i_min) * i_mult;
        }

        clip_channel_f(s_channel, channel_size, 0., 1.);
        clip_channel_f(i_channel, channel_size, 0., 255.);

        unsigned char **rgb = conv_hsi_to_rgb(hsi[0], hsi[1], hsi[2], channel_size);

        free(hsi[0]);
        free(hsi[1]);
        free(hsi[2]);
        free(hsi);

        memcpy(r_channel, rgb[0], channel_size * sizeof(unsigned char));
        memcpy(g_channel, rgb[1], channel_size * sizeof(unsigned char));
        memcpy(b_channel, rgb[2], channel_size * sizeof(unsigned char));
        free(rgb[0]);
        free(rgb[1]);
        free(rgb[2]);
        free(rgb);
    }

    cv::merge(bgr_split, 3, mat);

    memcpy(arr, mat.data, height * width * depth);
    return 0;
}
