#include "vision_link.h"

#include <condition_variable>
#include <cstdio>
#include <ctime>
#include <mutex>
#include <signal.h>
#include <thread>

#include <opencv2/opencv.hpp>

#include "conf/vehicle.hpp"
#include "vision/c/camera_message_framework.hpp"
#define MAX_FPS 15

class VisionLinkImp : public VisionLink {
  public:
    virtual ~VisionLinkImp();
    int init(const std::string &cam_name, unsigned int width,
                                          unsigned int height) override;
    void post(unsigned char *image, timespec *acq_time) override;

  private:
    void worker();

    message_framework_p framework;
    cv::Mat input_image;
    cv::Mat output_image;

    char job = 0;
    bool ending = 0;
    timespec *acq_time;
    std::mutex mtx;
    std::condition_variable cv;
    std::chrono::milliseconds last_post = std::chrono::milliseconds(0);
    std::chrono::milliseconds interval = std::chrono::milliseconds(1000 / MAX_FPS);
};

VisionLinkImp::~VisionLinkImp() {
  if (framework != NULL) {
    std::unique_lock<std::mutex> lck(mtx);
    ending = 1;
	cv.notify_all();
    cleanup_message_framework(framework);
  }
}

int VisionLinkImp::init(const std::string &cam_name,
                     unsigned int width, unsigned int height) {
  input_image.create(height, width, CV_8UC4);
  output_image.create(height, width, CV_8UC3);
  framework = create_message_framework(cam_name, width * height *3);
  if (framework == NULL) {
    fprintf(stderr, "ERROR: Failed to create camera message framework for %s ca"
                    "mera.\n",
            cam_name.c_str());
    return -1;
  }

  std::thread(&VisionLinkImp::worker, this).detach();

  return 0;
}

void bus_error_handler(int sig_number, siginfo_t *info, void *context) {
  /* This happens sometimes when vision is started. I believe it happens
     due to a race condition in how vision opens the memory mapped file
     that this module writes to (using write_frame, from
     camera-message-framework). The SIGBUS signal is raised from the memcpy
     operation inside write_frame. */
  fprintf(stderr, "!!! BUS ERROR CAUGHT !!! (on address %p) "
                  "PLEASE DEBUG ME\n", info->si_addr);
}

void VisionLinkImp::worker() {
  struct sigaction handler;
  handler.sa_sigaction = bus_error_handler;
  handler.sa_flags = SA_SIGINFO;
  sigemptyset(&handler.sa_mask);
  // Register the SIGBUS signal handler.
  sigaction(SIGBUS, &handler, NULL);

  while (1) {
    std::unique_lock<std::mutex> lck(mtx);
    while (!job && !ending) {
      cv.wait(lck);
    }

    if (ending) {
      break;
    }

    cv::cvtColor(input_image, output_image, CV_BGRA2BGR);
    cv::flip(output_image, output_image, 0);
    write_frame(framework, output_image.ptr(),
                acq_time->tv_sec * 1000 + acq_time->tv_nsec / 1000000,
                output_image.cols, output_image.rows, output_image.channels());
    job = 0;
  }
}

void VisionLinkImp::post(unsigned char *image, timespec *acq_time_new) {
  std::chrono::milliseconds now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
  // Limit FPS
  if (now - last_post < interval) {
      return;
  }
  else {
      last_post = now;
  }
  std::unique_lock<std::mutex> lck(mtx);
  input_image = cv::Mat(input_image.size(), CV_8UC4, image);
  acq_time = acq_time_new;
  job = 1;
  cv.notify_all();
}

// Used to load this library dynamically at run time.
// See www.tldp.org/HOWTO/C++-dlopen/thesolution.html
extern "C" VisionLink *create_link() {
  return new VisionLinkImp();
}

extern "C" void destroy_link(VisionLink *link) {
  delete link;
}

extern "C" auto get_vehicle_cameras() {
  cuauv::conf::vehicle sub = cuauv::conf::load_vehicle();
  std::vector<struct visualizer_camera> cams;

  for (const auto &cam : sub.cameras) {
    glm::vec3 position(cam.position.x(), cam.position.y(), cam.position.z());
    glm::vec3 orientation_hpr(cam.orientation_hpr.x(), cam.orientation_hpr.y(),
                              cam.orientation_hpr.z());

    struct visualizer_camera vis_cam( {
      cam.tag, cam.width, cam.height, position, orientation_hpr,
      cam.sensor_width, cam.sensor_height, cam.focal_length
    });

    cams.emplace_back(std::move(vis_cam));
  }

  return std::move(cams);
}
