#pragma once

#include <cstdlib>
#include <string>

#include <GL/gl.h>

namespace aslam_comm { 

class ASLAMComm {
  public:
    int connect(const char *addr="127.0.0.1");
    bool fill_data(GLfloat *position, GLfloat *color, int n_points);

    int n_points = -1;

  private:
    int reconnect();
    int get_n_points();
    int safe_recv(uint8_t *buf, int bytes);
    void report_error(std::string msg);

    int sockfd;
    std::string addr;
    float rgb[3] = { 0.0, 0.0, 0.0 };
    std::string prev_error;
};

}
