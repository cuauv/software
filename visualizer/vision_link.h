#ifndef _VISION_LINK_H_
#define _VISION_LINK_H_

#include <chrono>
#include <string>
#include <vector>

#include <glm/vec3.hpp>
#include <glm/gtc/quaternion.hpp>

class timespec;

class VisionLink {
  public:
    virtual ~VisionLink() {}
    virtual int init(const std::string &cam_name, unsigned int width,
                                                  unsigned int height) = 0;
    virtual void post(unsigned char *image, timespec *acq_time) = 0;
};

// We don't want to depend on Eigen in this header file,
// so we wrap the results from load_vehicle inside a struct
// that uses GLM vectors instead of Eigen vectors.
struct visualizer_camera {
    std::string tag;
    int width;
    int height;
    glm::vec3 position;
    glm::vec3 orientation_hpr;

    // Sensor sizes and focal length are in millimeters.
    double sensor_width;
    double sensor_height;
    double focal_length;
};

#endif
