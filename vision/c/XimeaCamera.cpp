#include "XimeaCamera.hpp"
#include "CaptureSource.hpp"

#include "camera_filters.hpp"

#include <m3api/xiApi.h>
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include <cstdint>
#include <memory>
#include <experimental/optional> //TODO: switch this to C++17

CaptureSource *get_cap(struct capture_source_params *params) {
  return new XimeaCamera(params);
}


struct XimeaCamera::XimeaCameraImpl {
  struct capture_source_params *params;

  size_t native_width, native_height;
  size_t width, height;
  bool resize;

  uint32_t exposure;
  bool auto_wb;
  float gamma_y, gamma_c, wb_kr, wb_kb, wb_kg, sharpness;
  HANDLE xiH;
  std::unique_ptr<optimal_camera_matrix> undistort_matrix;
  XI_IMG image;

  XimeaCameraImpl(struct capture_source_params *params)
    : params(params),
      width(params->width),
      height(params->height),
      exposure(8000),
      auto_wb(false),
      gamma_y(1),
      gamma_c(0.5),
      wb_kr(1.2),
      wb_kb(1.2),
      wb_kg(0.5),
      sharpness(4),
      undistort_matrix(std::make_unique<optimal_camera_matrix>())
  {}
};

XimeaCamera::XimeaCamera(struct capture_source_params *params)
  : CaptureSource(30, params->direction),
    pimpl(new XimeaCameraImpl(params)) {
  initUndistortMap(pimpl->undistort_matrix.get(), "ximea", pimpl->width, pimpl->height);

  memset(&pimpl->image, 0, sizeof(XI_IMG));
  pimpl->image.size = sizeof(XI_IMG);
}

cv::Size XimeaCamera::get_output_size() {
  return cv::Size(pimpl->width, pimpl->height);
}

void XimeaCamera::destroy_capture_source() {
  xiCloseDevice(pimpl->xiH);
}

bool XimeaCamera::setup_capture_source() {
  pimpl->xiH = NULL;
  XI_RETURN stat = XI_OK;

  DWORD dwNumberOfDevices = 0;
  stat = xiGetNumberDevices(&dwNumberOfDevices);
  if (stat != XI_OK) {
    std::cout << "Error: xiGetNumberDevices (no camera found): " << stat
              << std::endl;
    return false;
  }

  if (!dwNumberOfDevices) {
    std::cout << "Error: No cameras found" << std::endl;
    return false;
  }

  stat = xiOpenDevice(0, &pimpl->xiH);

  if (stat != XI_OK) {
    std::cout << "Error: No cameras found: " << stat << std::endl;
    return false;
  }

  // Get native resolution
  int width, height;
  stat = xiGetParamInt(pimpl->xiH, XI_PRM_HEIGHT, &height);
  if (stat != XI_OK) {
    std::cout << "Error: xiGetParam (height): " << stat << std::endl;
  }
  stat = xiGetParamInt(pimpl->xiH, XI_PRM_WIDTH, &width);
  if (stat != XI_OK) {
    std::cout << "Error: xiGetParam (width): " << stat << std::endl;
  }
  pimpl->native_width = width;
  pimpl->native_height = height;
  std::cout << "Native width and height are " << pimpl->native_width << " " << pimpl->native_height << std::endl;

  if (pimpl->native_width != pimpl->width ||
      pimpl->native_height != pimpl->height) {
    pimpl->resize = true;
  } else {
    pimpl->resize = false;
  }

  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_GAMMAY, pimpl->gamma_y);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (gammay set): " << stat << std::endl;
    return false;
  }


  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_GAMMAC, pimpl->gamma_c);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (gammac set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamInt(pimpl->xiH, XI_PRM_AUTO_WB, pimpl->auto_wb);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (auto white balance set): " << stat
              << std::endl;
    return false;
  }

  stat = xiSetParamInt(pimpl->xiH, XI_PRM_EXPOSURE, pimpl->exposure);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (exposure set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_WB_KR, pimpl->wb_kr);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (wb kr set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_WB_KB, pimpl->wb_kb);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (wb kb set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_WB_KG, pimpl->wb_kg);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (wb kg set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamFloat(pimpl->xiH, XI_PRM_SHARPNESS, pimpl->sharpness);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (sharpness set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamInt(pimpl->xiH, XI_PRM_BUFFER_POLICY, XI_BP_UNSAFE);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (buffer policy set): " << stat << std::endl;
    return false;
  }

  stat = xiSetParamInt(pimpl->xiH, XI_PRM_IMAGE_DATA_FORMAT, XI_RGB24);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (format set): " << stat << std::endl;
    return false;
  }

  // std::cout << "Setting width to " << pimpl->width << std::endl;
  // stat = xiSetParamInt(pimpl->xiH, XI_PRM_WIDTH, pimpl->width);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (width set): " << stat << std::endl;
    return false;
  }

  // std::cout << "Setting height to " << pimpl->height << std::endl;
  // stat = xiSetParamInt(pimpl->xiH, XI_PRM_HEIGHT, pimpl->height);

  if (stat != XI_OK) {
    std::cout << "Error: xiSetParam (height set): " << stat << std::endl;
    return false;
  }

  stat = xiStartAcquisition(pimpl->xiH);

  if (stat != XI_OK) {
    std::cout << "Error: xiStartAcquisition: " << stat << std::endl;
    return false;
  }

  return true;
}

std::experimental::optional<std::pair<cv::Mat, long>> XimeaCamera::acquire_next_image() {
  XI_RETURN stat;
  // Get the next image
  stat = xiGetImage(pimpl->xiH, 5000, &pimpl->image);
  if (stat != XI_OK)
    return std::experimental::nullopt;

  long acq_time = get_time();
  cv::Mat orig = cv::Mat(pimpl->native_width, pimpl->native_height, CV_8UC3, pimpl->image.bp);
  if (pimpl->params->rotate180) {
    cv::flip(orig, orig, -1);
  }

  if (pimpl->resize) {
    cv::Mat resized =
      cv::Mat(pimpl->width, pimpl->height, CV_8UC3);

    cv::resize(orig, resized, resized.size());
    return std::make_pair(resized, acq_time);
  } else {
    return std::make_pair(orig, acq_time);
  }
}
