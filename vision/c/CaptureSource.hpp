#pragma once

#include "camera_message_framework.hpp"

#include <opencv2/core/core.hpp>

#include <csignal>
#include <iostream>
#include <chrono>
#include <thread>
#include <experimental/optional> //TODO: switch this to C++17

struct capture_source_params {
  std::string direction;
  unsigned int camera_id;
  std::string configuration;
  bool rotate180;
  unsigned int width;
  unsigned int height;
};

class CaptureSource;
CaptureSource *get_cap(struct capture_source_params *params);

class CaptureSource {
private:
  int m_max_fps;
  message_framework_p m_framework;
  volatile bool running;

protected:
  std::string m_direction;

  virtual cv::Size get_output_size() = 0;
  virtual bool setup_capture_source() = 0;
  virtual void destroy_capture_source() = 0;
  virtual std::experimental::optional<std::pair<cv::Mat, long>> acquire_next_image() = 0;

  long get_time() {
    return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
  }

  CaptureSource(int max_fps, std::string direction)
    : m_max_fps(max_fps),
      m_framework(NULL),
      running(true),
      m_direction(direction)
       {
  }

public:
  void terminate() {
    running = false;
  }

  void run() {
    if (!setup_capture_source()) {
      std::cout << "Could not initialize capture source" << std::endl;
      return;
    }

    cv::Size output_size = get_output_size();
    size_t full_image_dimensions = output_size.width * output_size.height * 3;
    m_framework = create_message_framework(m_direction, full_image_dimensions);

    if (m_framework == NULL) {
      std::cout << "Could not initialize message framework" << std::endl;
      return;
    }

    std::cout << "Set up camera. Starting image acquisition" << std::endl;

    long last_acq_time = 0;

    while (running) {
      std::experimental::optional<std::pair<cv::Mat, long>> next_image_op = acquire_next_image();

      if (!next_image_op) {
        std::cout << "Error acquiring image! Trying again." << std::endl;
        continue;
      }

      cv::Mat next_image = next_image_op.value().first;
      long acq_time = next_image_op.value().second;

      write_frame(m_framework, next_image.data, acq_time, output_size.width, output_size.height, 3);

      long curr_time = get_time();
      long time_since_last_acq = curr_time - last_acq_time;
      long min_acq_time = 1000 / m_max_fps;
      auto sleep_time =  std::chrono::milliseconds(min_acq_time - time_since_last_acq);
      if (sleep_time > std::chrono::milliseconds(0)) {
        std::this_thread::sleep_for(sleep_time);
      }
      last_acq_time = acq_time;
    }
    destroy_capture_source();
    std::cout << "Cleaned up capture source" << std::endl;
    cleanup_message_framework(m_framework);
    std::cout << "Cleaned up message framework" << std::endl;
  }
};
