#include "CaptureSource.hpp"

#include <cstdio>
#include <signal.h>
#include <sstream>
#include <string>

// this is global to allow for signal handling, otherwise we would have to
//  capture it within main() somehow, which from what I can tell is impossible
CaptureSource *cap;

int main(int argc, char* argv[]) {
  struct capture_source_params params;

  if (argc > 1) {
    params.direction = argv[1];
  }

  if (argc > 2) {
    try {
      params.camera_id = std::stoi(argv[2]);
    } catch (std::invalid_argument e) {
      fprintf(stderr, "Second argument must be a camera ID (integer)\n");
      return -1;
    }
  }

  if (argc > 3) {
    params.configuration = argv[3];
  }

  bool size_given = false;
  params.rotate180 = false;
  for (int i = 4; i < argc; i++) {
    std::string arg(argv[i]);
    if (arg == "--rotate180") {
      params.rotate180 = true;
    }
    else if (arg.substr(0, 5) == "size=") {
      std::istringstream ss(arg.substr(5));
      char c;
      ss >> params.width;
      ss >> c;
      ss >> params.height;
      size_given = c == 'x' && params.width != 0 && params.height != 0;
    }
  }

  if (!size_given) {
    std::cerr << "No camera size given, provide as size=widthxheight. Aborting."
              << std::endl;
    return 1;
  }

  cap = get_cap(&params);

  auto signal_handler = [] (int sig) {
    std::cout << "Terminating capture source" << std::endl;
    cap->terminate();
  };

  signal(SIGINT, signal_handler);
  signal(SIGTERM, signal_handler);

  cap->run();

  return 0;
}
