#pragma once

#include <opencv2/opencv.hpp>
#include <string.h>

typedef struct {
  cv::Mat map1;
  cv::Mat map2;
} optimal_camera_matrix;

int initUndistortMap(optimal_camera_matrix *dst, std::string name, int width, int height);
