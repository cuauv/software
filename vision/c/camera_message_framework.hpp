#pragma once

#define FRAMEWORK_NAME_MAX 255

#include <cstddef>
#include <string>
#include <memory>
#include <stdint.h>

// Return values of read_frame.
#define FRAMEWORK_QUIT 1
#define FRAMEWORK_DELETED 2

extern "C" {
  typedef struct message_framework message_framework;
  typedef message_framework* message_framework_p;

  struct frame {
    unsigned char* data;
    uint32_t last_frame;
    size_t width;
    size_t height;
    size_t depth;
    uint64_t acq_time;
  };

  /**
   * Create a message framework in the given direction.
   * Returns: NULL if the message framework could not be created (i.e. if the
   *  name was already in use, or if there was some other failure), or a pointer
   *  to the message framework.
   */
  message_framework_p create_message_framework(std::string direction,
                                               size_t max_size);
  message_framework_p create_message_framework_from_cstring(const char* direction,
                                                            size_t max_size);
  message_framework_p access_message_framework(std::string direction);
  message_framework_p access_message_framework_from_cstring(const char* direction);
  bool write_frame(message_framework_p framework, unsigned char* data,
                   unsigned long acquisition_time_in_ms, size_t width,
                   size_t height, size_t depth);
  int read_frame(struct frame* frame, message_framework_p framework);
  void kill_message_framework(message_framework_p framework);
  void cleanup_message_framework(message_framework_p framework);
  size_t get_buffer_size(message_framework_p framework);
}
