#include "aslam_comm.h"

#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "misc/utils.h"

#define ASLAM_PORT 8888

namespace aslam_comm { 

#define HDR_POINT 64
#define HDR_COLOR 65
#define POINT_RECV 16
#define COLOR_RECV 12

// ASLAM PARTICLE PROTOCOL
// struct msg {
//   uint8_t header // This is either HDR_POINT or HDR_COLOR
//   float vec[3]   // Position of point (if HDR_POINT) or color (if HDR_COLOR)
//   uint8          // Weight of point (only if HDR_POINT)
// };

int ASLAMComm::connect(const char *addr_) {
  addr = addr_;
  return reconnect();
}

int ASLAMComm::reconnect() {
  if (sockfd) {
    close(sockfd);
  }
  int ret = socket_connect(sockfd, addr.c_str(), ASLAM_PORT);
  if (ret) {
    return ret;
  }

  int requested_points = get_n_points();
  if (requested_points >= 0) {
    n_points = requested_points;
  }

  return 0;
}

int ASLAMComm::get_n_points() {
  uint32_t n;
  if (safe_recv((uint8_t *)&n, sizeof(uint32_t))) {
    return -69;
  }

  return n;
}

void ASLAMComm::report_error(std::string msg) {
  if (prev_error != msg) {
    fprintf(stderr, "%s\n", msg.c_str());
    prev_error = msg;
  }
}

int ASLAMComm::safe_recv(uint8_t *buf, int bytes) {
  int total_bytes = 0;
  while (total_bytes < bytes) {
    int recvd = recv(sockfd, &buf[total_bytes], bytes - total_bytes, 0);
    if (recvd <= 0) {
      report_error("WARNING: Receive error in ASLAM Comm.");
      reconnect();
      return -1;
    }
    else {
      total_bytes += recvd;
    }
  }

  if (total_bytes > bytes) {
    report_error("WARNING: ASLAM Comm received more bytes than expected!");
  }

  return 0;
}

bool ASLAMComm::fill_data(GLfloat *position, GLfloat *color, int max_points) {
  uint8_t buf[POINT_RECV]; // Should be the max message length.
  usleep(16000);

  if (max_points != n_points) {
    report_error("WARNING: ASLAM sends " + std::to_string(n_points) + " but we "
                 "expect " + std::to_string(max_points) + "! Please restart.");
    return false;
  }

  // Initiate ASLAM data transmission.
  if (send(sockfd, &sockfd, 1, MSG_NOSIGNAL) <= 0) {
    report_error("WARNING: ASLAM send failed.");
    reconnect();
    return false;
  }

  for (int ind = 0; ind < max_points; ) {
    if (safe_recv(buf, 1)) {
      return false;
    }

    if (buf[0] == HDR_POINT) {
      if (safe_recv(buf, POINT_RECV)) {
        return false;
      }

      memcpy(&position[3*ind], buf, sizeof(float) * 3);

      float weight;
      memcpy(&weight, &buf[12], sizeof(float));

      // TODO Adjust color based on weight.
      memcpy(&color[3*ind], rgb, sizeof(float) * 3);
      ind++;
    }

    else if (buf[0] == HDR_COLOR) {
      if (safe_recv(buf, COLOR_RECV)) {
        return false;
      }
      memcpy(rgb, buf, sizeof(float) * 3);
    }

    else {
      report_error("WARNING: Invalid message header from ASLAM!");
      return false;
    }
  }

  report_error("ASLAM Comms is all good now!");
  return true;
}

}
