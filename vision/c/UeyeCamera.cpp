#include "CaptureSource.hpp"
#include "UeyeCamera.hpp"

#include "misc/utils.h"

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <ueye.h>
#include <libshm/c/shm.h>

#include <iostream>
#include <string>
#include <experimental/optional> //TODO: switch this to C++17

#define CAMERA_IMAGE_BUFFER_LEN 10

CaptureSource *get_cap(struct capture_source_params *params) {
  return new UeyeCamera(params);
}

typedef struct _ueye_image {
  char *buffer;
  int id;
} ueye_image;

struct UeyeCamera::UeyeCameraImpl {
  struct capture_source_params *params;
  cv::Mat result;
  char* last_buffer_loc;
  HIDS m_camera;
  ueye_image images[CAMERA_IMAGE_BUFFER_LEN];

  UeyeCameraImpl(struct capture_source_params *params)
    : params(params),
      result(cv::Mat(params->width, params->height, CV_8UC3)),
      last_buffer_loc(NULL) {}
};


UeyeCamera::UeyeCamera(struct capture_source_params *params)
  : CaptureSource(10, params->direction),
    pimpl(new UeyeCameraImpl(params)) {}

void UeyeCamera::destroy_capture_source() {
  std::cout << "Deconstructing UEye camera" << std::endl;
  int ret;
  if ((ret = is_ExitCamera(pimpl->m_camera)) != IS_SUCCESS) {
    std::cout << "Failed to exit camera: " << ret << std::endl;
    return;
  }
}

cv::Size UeyeCamera::get_output_size() {
  return cv::Size(pimpl->params->width, pimpl->params->height);
}

bool UeyeCamera::setup_capture_source() {
  int num_cameras;
  if (is_GetNumberOfCameras(&num_cameras) != IS_SUCCESS) {
    std::cout << "Could not get number of cameras" << std::endl;
  }

  // initialize camera list struct to the correct size
  size_t camera_list_size = sizeof(DWORD) + num_cameras * sizeof(UEYE_CAMERA_INFO);
  UEYE_CAMERA_LIST *camera_list = (UEYE_CAMERA_LIST*) new char[camera_list_size];
  camera_list->dwCount = num_cameras;

  if (is_GetCameraList(camera_list) != IS_SUCCESS) {
    std::cout << "Could not get camera list" << std::endl;
    return false;
  }

  int camera_idx = -1;
  for (int i = 0; i < num_cameras; i++) {
    if (camera_list->uci[i].dwCameraID == pimpl->params->camera_id) {
      camera_idx = i;
      break;
    }
  }
  if (camera_idx == -1) {
    std::cout << "Invalid camera id " << pimpl->params->camera_id << std::endl;
    return false;
  }

  UEYE_CAMERA_INFO cam_info = camera_list->uci[camera_idx];
  if (cam_info.dwInUse) {
    std::cout << "Camera in use" << std::endl;
    return false;
  }
  pimpl->m_camera = (HIDS) (cam_info.dwDeviceID | IS_USE_DEVICE_ID);

  if (is_InitCamera(&pimpl->m_camera, NULL) != IS_SUCCESS) {
    std::cout << "Could not open IDS camera" << std::endl;
    return false;
  }

  if (is_EnableAutoExit(pimpl->m_camera, IS_ENABLE_AUTO_EXIT) != IS_SUCCESS) {
    std::cout << "Failed to enable auto_exit" << std::endl;
    return false;
  }

  unsigned int uInitialParameterSet = IS_CONFIG_INITIAL_PARAMETERSET_NONE;
  if (is_Configuration(IS_CONFIG_INITIAL_PARAMETERSET_CMD_GET, &uInitialParameterSet, sizeof(unsigned int)) != IS_SUCCESS) {
    std::cout << "Could not set initial parameters" << std::endl;
    return false;
  }

  std::string config = getBinDir() + "../c/configs/" + pimpl->params->configuration + ".ini";
  std::wstring wconfig = std::wstring(config.begin(), config.end());
  int ret;
  if ((ret = is_ParameterSet(pimpl->m_camera, IS_PARAMETERSET_CMD_LOAD_FILE,
                             (void*) wconfig.c_str(), sizeof(const char*))) != IS_SUCCESS) {
    std::cout << "Could not load camera config (" << pimpl->params->configuration << "): " << ret << std::endl;
    return false;
  }

  if (is_SetColorMode(pimpl->m_camera, IS_CM_BGR8_PACKED) != IS_SUCCESS) {
    std::cout << "Failed to set color mode" << std::endl;
    return false;
  }

  if (is_ClearSequence(pimpl->m_camera) != IS_SUCCESS) {
    std::cout << "Failed to clear camera sequence" << std::endl;
    return false;
  }

  UINT num_entries;
  if (is_ImageFormat(pimpl->m_camera, IMGFRMT_CMD_GET_NUM_ENTRIES, &num_entries, 4) != IS_SUCCESS) {
    std::cout << "Failed to get number of image formats" << std::endl;
  }

  UINT format_list_size = sizeof(IMAGE_FORMAT_LIST) + ((num_entries - 1) * sizeof(IMAGE_FORMAT_INFO));
  IMAGE_FORMAT_LIST* format_list = (IMAGE_FORMAT_LIST*) malloc(format_list_size);
  format_list->nSizeOfListEntry = sizeof(IMAGE_FORMAT_INFO);
  format_list->nNumListElements = num_entries;

  UINT retval = is_ImageFormat(pimpl->m_camera, IMGFRMT_CMD_GET_LIST, format_list, format_list_size);

  if (retval != IS_SUCCESS) {
      std::cout << "Failed to get image formats, error code:  " << retval << std::endl;
      return false;
  }

  IMAGE_FORMAT_INFO native_format = format_list->FormatInfo[0];
  UINT native_format_id = format_list->FormatInfo[0].nFormatID;

  // Loop throught the supported image resolutions (called formats by camera
  // API), printing them out and if one matches the requested resolution,
  // remember that format ID
  std::cout << "Supported image resolutions: ";
  IMAGE_FORMAT_INFO format_info;
  bool found = false;
  for (UINT i = 0; i < num_entries; i++) {
    format_info = format_list->FormatInfo[i];
    std::cout << format_info.nWidth <<  "x" << format_info.nHeight << ", ";

    // Remember this format ID if it is the same as the requested resolution
    if (format_info.nWidth == pimpl->params->width &&
        format_info.nHeight == pimpl->params->height) {
      found = true;
      native_format_id = format_info.nFormatID;
      native_format = format_list->FormatInfo[i];
    }
  }
  std::cout << std::endl;

  if (!found) {
    std::cerr << "Unable to set Ueye camera with ID " << pimpl->params->camera_id <<
                 " to a resolution of " << pimpl->params->width << "x"
              << pimpl->params->height << std::endl;
    return false;
  }

  std::cout << "Setting to requested resolution " << native_format.nWidth << "x" << native_format.nHeight << std::endl;

  retval =  is_ImageFormat(pimpl->m_camera, IMGFRMT_CMD_SET_FORMAT, &native_format_id, 4);
  if (retval != IS_SUCCESS) {
      std::cout << "Failed to set camera to native resolution, error code: " << retval << std::endl;
      return false;
  }

  // Set camera dimensions in shm so other software knows what they are (e.g. missions
  // that try to center a target in the camera)
  // TODO: use dynamic shm to do this by reading from camera direction
  shm_init();
  if (this->m_direction.compare("forward") == 0) {
    shm_set(camera, forward_width, (int) pimpl->params->width);
    shm_set(camera, forward_height, (int) pimpl->params->height);
  } else if (this->m_direction.compare("downward") == 0) {
    shm_set(camera, downward_width, (int) pimpl->params->width);
    shm_set(camera, downward_height, (int) pimpl->params->height);
  } else {
      std::cout << "Unsupported camera direction " << this->m_direction << ", must be one of 'forward' or 'downward'" << std::endl;
      return false;
  }

  // Set area of interest to full image
  IS_RECT rect_aoi;
  rect_aoi.s32X = (INT) 0;
  rect_aoi.s32Y = (INT) 0;
  rect_aoi.s32Width = (INT) pimpl->params->width;
  rect_aoi.s32Height = (INT) pimpl->params->height;

  retval = is_AOI(pimpl->m_camera, IS_AOI_IMAGE_SET_AOI, (void *) &rect_aoi, (UINT) sizeof(rect_aoi));
  if (retval != 0) {
    std::cout << "Failed to set image area of interest, error code: " << retval << std::endl;
    return false;
  }

  retval = is_AOI(pimpl->m_camera, IS_AOI_AUTO_BRIGHTNESS_SET_AOI, (void *) &rect_aoi, (UINT) sizeof(rect_aoi));
  if (retval != 0) {
    std::cout << "Failed to set image area of interest for auto brightness, error code: " << retval << std::endl;
    return false;
  }

  retval = is_AOI(pimpl->m_camera, IS_AOI_AUTO_WHITEBALANCE_SET_AOI, (void *) &rect_aoi, (UINT) sizeof(rect_aoi));
  if (retval != 0) {
    std::cout << "Failed to set image area of interest for auto whitebalance, error code: " << retval << std::endl;
    return false;
  }

  for (unsigned int i = 0; i < CAMERA_IMAGE_BUFFER_LEN; i++) {
    if (is_AllocImageMem (pimpl->m_camera, pimpl->params->width, pimpl->params->height,
                          24, &pimpl->images[i].buffer,
                          &pimpl->images[i].id) != IS_SUCCESS) {
      std::cout << "Failed to allocate image memory" << std::endl;
      return false;
    }
    if (is_AddToSequence (pimpl->m_camera, pimpl->images[i].buffer,
                          pimpl->images[i].id) != IS_SUCCESS) {
      std::cout << "Failed to add image to sequence" << std::endl;
      return false;
    }
  }

  if (is_EnableEvent (pimpl->m_camera, IS_SET_EVENT_FRAME) != IS_SUCCESS) {
    std::cout << "Failed to enable frame event" << std::endl;
    return false;
  }

  if (is_CaptureVideo(pimpl->m_camera, IS_DONT_WAIT) != IS_SUCCESS) {
    std::cout << "Failed to start video capture" << std::endl;
    return false;
  }

  return true;
}

std::experimental::optional<std::pair<cv::Mat, long>> UeyeCamera::acquire_next_image() {
  if (pimpl->last_buffer_loc != NULL) {
    int ret;
    if ((ret = is_UnlockSeqBuf(pimpl->m_camera, IS_IGNORE_PARAMETER,
                               pimpl->last_buffer_loc)) != IS_SUCCESS) {
      std::cout << "Failed to unlock last image buffer: " << ret << std::endl;
    }
    pimpl->last_buffer_loc = NULL;
  }

  if ((is_WaitEvent(pimpl->m_camera, IS_SET_EVENT_FRAME, 1000)) != IS_SUCCESS) {
    std::cout << "Camera frame timeout!" << std::endl;
    return std::experimental::nullopt;
  }
  //mem is a dummy variable - points to the last image seq used by the camera, but that might not have been the last seq that we used, so it's not particularly useful
  char *buffer = NULL, *mem = NULL;
  int dummy;
  is_GetActSeqBuf(pimpl->m_camera, &dummy, &mem, &buffer);

  if (is_LockSeqBuf(pimpl->m_camera, IS_IGNORE_PARAMETER, buffer) != IS_SUCCESS) {
    std::cout << "Failed to lock image buffer" << std::endl;
    return std::experimental::nullopt;
  }

  pimpl->last_buffer_loc = buffer;
  pimpl->result.data = (unsigned char*) buffer;
  if (pimpl->params->rotate180) {
    cv::flip(pimpl->result, pimpl->result, -1);
  }
  return std::make_pair(pimpl->result, get_time());
}
