#include "camera_message_framework.hpp"

#include <cstdlib>
#include <cstddef>
#include <cstdio>
#include <sys/mman.h>
#include <fcntl.h>
#include <string>
#include <sstream>
#include <atomic>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <iostream>
#include <stdint.h>
#include <semaphore.h>
#include <sys/time.h>

#include "misc/utils.h"

#define FILE_ADDRESS_BASE "/dev/shm/auv_visiond-"
#define BUFFER_COUNT 3

struct image_metadata {
  size_t width;
  size_t height;
  size_t depth;
  uint64_t acquisition_time;
  pthread_rwlock_t rwlock;
};

struct message_framework_internal {
  uint32_t frame;
  pthread_cond_t cond;
  pthread_mutex_t cond_mutex;
  struct image_metadata metadata[BUFFER_COUNT];
  size_t image_buffer_size;
  pid_t owner;
  unsigned char images[]; // this _must_ be last
} typedef message_framework_internal;

struct message_framework {
  bool live;
  std::atomic_uint_least32_t num_accessors;
  std::string filename;
  message_framework_internal *framework;
};

size_t calculate_map_size(size_t max_image_size) {
  return sizeof(message_framework_internal) + max_image_size*BUFFER_COUNT;
}

bool write_frame(message_framework_p framework_w, unsigned char* data,
                 uint64_t acquisition_time, size_t width, size_t height,
                 size_t depth) {
  if (!framework_w->live) {
    return false;
  }

  message_framework_internal *framework = framework_w->framework;

  uint32_t buffer_to_write_to = (framework->frame + 1) % BUFFER_COUNT;
  struct image_metadata *metadata = &framework->metadata[buffer_to_write_to];

  // grab the write lock
  pthread_rwlock_wrlock(&metadata->rwlock);
  // write the image and corresponding metadata
  memcpy(&framework->images[framework->image_buffer_size*buffer_to_write_to], data,
         width * height * depth);
  metadata->width = width;
  metadata->height = height;
  metadata->depth = depth;
  metadata->acquisition_time = acquisition_time;
  // release the write lock
  pthread_rwlock_unlock(&metadata->rwlock);

  framework->frame++;
  // notify all watchers that a new image has been posted
  pthread_cond_broadcast(&framework->cond);

  return true;
}

int read_frame(struct frame* frame, message_framework_p framework_w) {
  if (!framework_w->live) {
    return FRAMEWORK_QUIT;
  }

  message_framework_internal *framework = framework_w->framework;
  frame->data = (unsigned char*) realloc(frame->data, framework->image_buffer_size);
  pthread_mutex_t *mutex = &framework->cond_mutex;
  pthread_cond_t *cond = &framework->cond;

  pthread_mutex_lock(mutex);
  struct timespec timeToWait;
  struct timeval now;
  while (framework->frame == frame->last_frame) {
    // wait 100ms at most for each wait. this lets us avoid any esoteric
    // timing/thread related bugs at a small cost. if we prove all of our
    // synchronization code here to be correct, this won't be necessary.
    // until then, it's best to keep this in
    gettimeofday(&now,NULL);
    timeToWait.tv_sec = now.tv_sec;
    timeToWait.tv_nsec = (now.tv_usec+1000UL*100)*1000UL;
    if (timeToWait.tv_nsec >= 1000000000L) {
      timeToWait.tv_sec += 1;
      timeToWait.tv_nsec -= 1000000000L;
    }

    pthread_cond_timedwait(cond, mutex, &timeToWait);

    // if we should no longer be running, cleanly exit
    if (!framework_w->live) {
      pthread_mutex_unlock(mutex);
      return FRAMEWORK_QUIT;
    }

    if (!framework->owner) {
      pthread_mutex_unlock(mutex);
      std::cout << "Framework is abandoned!" << std::endl;
      return FRAMEWORK_DELETED;
    }
  }

  pthread_mutex_unlock(mutex);

  // read from the newly written frame
  uint32_t buffer_to_read_from = framework->frame % BUFFER_COUNT;
  struct image_metadata *metadata = &framework->metadata[buffer_to_read_from];
  pthread_rwlock_rdlock(&metadata->rwlock);
  frame->last_frame = framework->frame;
  frame->acq_time = metadata->acquisition_time;
  size_t width = metadata->width;
  size_t height = metadata->height;
  size_t depth = metadata->depth;
  frame->width = width;
  frame->height = height;
  frame->depth = depth;

  memcpy(frame->data, &framework->images[framework->image_buffer_size*buffer_to_read_from], width * height * depth);
  pthread_rwlock_unlock(&metadata->rwlock);
  return 0;
}

message_framework_p initialize_message_framework(message_framework_internal *framework) {
  // TODO Use a smart pointer here.
  message_framework_p framework_w = new message_framework;
  framework_w->framework = framework;
  framework_w->live = true;
  framework_w->num_accessors = 1;
  return framework_w;
}

message_framework_p create_message_framework_from_cstring(const char *direction,
                                                         size_t max_image_size) {
  return create_message_framework(std::string(direction), max_image_size);
}

message_framework_p create_message_framework(std::string direction,
                                            size_t max_image_size) {
  // Apparently, "/" is the ONLY forbidden 8 bit character forbidden
  // in Linux filenames!
  if (direction.find('/') != std::string::npos) {
    std::cerr << "Framework name " << direction << " contains a \"/\", which is disallowed!" << std::endl;
    return NULL;
  }

  std::string file_address_s;
  file_address_s = FILE_ADDRESS_BASE + direction;
  char* file_address = (char*) file_address_s.c_str();

  if (access(file_address, F_OK) != -1) {
    //this line must come before the file is accessed
    bool in_use_by_any = file_opened(file_address);

    message_framework_p extant_framework = access_message_framework(direction);
    if (extant_framework != NULL && extant_framework->framework!=NULL) {
      pid_t prev_owner = extant_framework->framework->owner;

      // this line checks if the prev owner is still alive.
      // "On success (at least one signal was sent), zero is returned. On error, -1 is returned"
      // it will not actually kill the previous owner, since the signal is 0
      bool owned = !kill(prev_owner, 0);

      bool legal_size = extant_framework->framework->image_buffer_size == max_image_size;
      if (!owned && legal_size) {
        // reuse the old framework, so that nothing has to be recreated or relinked
        // TODO: this is a race condition (what if two message frameworks are created simultaneously)
        extant_framework->framework->owner = getpid();
        return extant_framework;
      } else if (in_use_by_any) {
        std::cout << "Could not create buffer " << direction << " for writing: ";
        if (owned)
          std::cout << direction << " is currently owned by some other process. "
                    << "Make sure that no other capture source is trying to access this file"
                    << std::endl;
        else if (!legal_size)
          std::cout << direction << " is currently being used by another process with an incompatible size. "
                    << "Please close all other processes that may be using it."
                    << std::endl;
        else
          std::cout << "unknown error occurred (illegal state of message framework)." << std::endl;
        return NULL;
      }
      else {
        // if not in use, unlink and fall through
        unlink(file_address);
      }
    } else {
      std::cout << "unknown error occurred (file exists but couldn't be accessed)." << std::endl;
      return NULL;
    }
  }

  int framework_file = open(file_address, O_RDWR | O_CREAT, S_IRWXU);
  if (framework_file == -1) {
    std::cout << "Failed to open " << file_address << ": " << errno << std::endl;
    return NULL;
  }
  size_t desired_size = calculate_map_size(max_image_size);

  if (ftruncate(framework_file, desired_size) == -1) {
    std::cout << "Failed to truncate the file to the desired length: "
              << errno << std::endl;
    return NULL;
  }

  // initialize shared memory framework

  message_framework_internal* framework = (message_framework_internal*) mmap(NULL, desired_size, PROT_READ | PROT_WRITE, MAP_SHARED, framework_file, 0);
  close(framework_file);

  framework->image_buffer_size = max_image_size;
  framework->frame = 0;
  framework->owner = getpid();

  pthread_condattr_t attrcond;
  pthread_condattr_init(&attrcond);
  pthread_condattr_setpshared(&attrcond, PTHREAD_PROCESS_SHARED);
  pthread_cond_init(&framework->cond, &attrcond);

  pthread_mutexattr_t attrmutex;
  pthread_mutexattr_init(&attrmutex);
  pthread_mutexattr_setpshared(&attrmutex, PTHREAD_PROCESS_SHARED);
  pthread_mutex_init(&framework->cond_mutex, &attrmutex);

  pthread_rwlockattr_t attrrwlock;
  pthread_rwlockattr_init(&attrrwlock);
  pthread_rwlockattr_setpshared(&attrrwlock, PTHREAD_PROCESS_SHARED);

  for (int i = 0; i < BUFFER_COUNT; i++)
    pthread_rwlock_init(&framework->metadata[i].rwlock, &attrrwlock);

  message_framework_p final_fmwk = initialize_message_framework(framework);
  final_fmwk->filename = file_address_s;
  return final_fmwk;
}

message_framework_p access_message_framework_from_cstring(const char *direction) {
  return access_message_framework(std::string(direction));
}

message_framework_p access_message_framework(std::string direction) {
  std::string file_address_s;
  file_address_s = FILE_ADDRESS_BASE + direction;
  char* file_address = (char*) file_address_s.c_str();

  int framework_file;
  if (access(file_address, F_OK) == -1)
    return NULL;

  framework_file = open(file_address, O_RDWR, S_IRWXU);

  size_t desired_size = lseek(framework_file, 0, SEEK_END);

  message_framework_internal *framework = (message_framework_internal*) mmap(NULL, desired_size, PROT_READ | PROT_WRITE, MAP_SHARED, framework_file, 0);
  close(framework_file);

  std::cout << "Initializing" << std::endl;

  message_framework_p final_fwmk = initialize_message_framework(framework);
  final_fwmk->filename = file_address_s;
  return final_fwmk;
}

size_t get_buffer_size(message_framework_p framework_w) {
  if (!framework_w->live)
    return -1;

  return framework_w->framework->image_buffer_size;
}

void kill_message_framework(message_framework_p framework_w) {
  framework_w->live = false;
  pthread_cond_broadcast(&framework_w->framework->cond);
}

void cleanup_message_framework(message_framework_p framework_w) {
  message_framework_internal *framework = framework_w->framework;
  framework_w->num_accessors--;

  if (framework_w->num_accessors == 0) {
    framework->owner = 0;
    munmap(framework, calculate_map_size(framework->image_buffer_size));
    remove(framework_w->filename.c_str());
    delete framework_w;
  }
}
