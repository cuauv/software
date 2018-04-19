#include <opencv2/core/utility.hpp>
#include "opencv2/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include "opencv2/highgui.hpp"
#include <signal.h>
#include <iostream>
#include <unistd.h>

#include "vision/c/camera_message_framework.hpp"

using namespace cv;

bool running = true;

void handle_sig(int sig) {
  running = false;
}

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout <<
      "Please provide a camera direction (e.g. \"forward\" or \"downward\")" <<
      std::endl;
    return 1;
  }
  char* direction = argv[1];

  signal(SIGINT, handle_sig);
  signal(SIGTERM, handle_sig);

  message_framework_p framework = access_message_framework(direction);
  if (framework == NULL) {
    std::cout << "No camera found in direction \"" << direction << "\"" << std::endl;
    return 1;
  }
  struct frame frame;
  frame.data = NULL;

  namedWindow(direction, 1);
  void* handle = cvGetWindowHandle(direction);

  while (waitKey(25) != 27 && handle != NULL && running) {
    handle = cvGetWindowHandle(direction);
    if (handle == NULL)
      break;

    if (!framework || read_frame(&frame, framework)) {
      framework = access_message_framework(direction);
      if (!framework) {
        usleep(100000);
        continue;
      }
    }

    std::cout << "got frame" << std::endl;
    Mat m(frame.height, frame.width, CV_8UC3, frame.data);
    imshow(direction, m);
    handle = cvGetWindowHandle(direction);
  }
}
