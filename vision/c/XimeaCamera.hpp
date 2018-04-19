#pragma once

#include "CaptureSource.hpp"

#include <opencv2/core/core.hpp>

#include <string>
#include <memory>

class XimeaCamera : public CaptureSource {
private:
  struct XimeaCameraImpl;
  std::unique_ptr<XimeaCameraImpl> pimpl;

public:
  XimeaCamera(struct capture_source_params *params);

protected:
  bool setup_capture_source();
  void destroy_capture_source();
  cv::Size get_output_size();
  std::experimental::optional<std::pair<cv::Mat, long>> acquire_next_image();
};
