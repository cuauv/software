#include "camera_message_framework.hpp"

#include <opencv2/imgproc/imgproc.hpp>

#include <stdio.h>
#include <string.h>
#include <string>
#include <dc1394/dc1394.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <semaphore.h>
#include <sys/time.h>

#include "misc/utils.h"

#define MAX_FPS 10
#define _MIN_IMAGE_TIME_IN_MS 1000 / MAX_FPS

bool capture_running = true;

void handle_sig(int sig) {
  capture_running = false;
}

int main(int argc, char *argv[]) {
  int width = 1024;
  int height = 768;

  if (argc < 2) {
    printf("Please provide a camera direction (e.g. \"forward\" or \"downward\"\n");
    return 1;
  }
  signal(SIGINT, handle_sig);
  signal(SIGTERM, handle_sig);

  std::string direction(argv[1]);

  message_framework_p framework = create_message_framework(direction, width*height*3);

  dc1394_t * dc1394_;
  dc1394camera_list_t * list_;
  dc1394camera_t *camera_;
  dc1394error_t err;
  dc1394video_frame_t *frame_, bayer_out0_, bayer_out1_;

  memset(&bayer_out0_, 0, sizeof(bayer_out0_));
  memset(&bayer_out1_, 0, sizeof(bayer_out1_));

  bayer_out0_.color_coding = DC1394_COLOR_CODING_RGB8;
  bayer_out1_.color_coding = DC1394_COLOR_CODING_RGB8;
  int i_buf = 0;

  bool do_bayer_ = false;
  unsigned int address_ = 0;
  int DMA_size_ = 10;
  dc1394_ = dc1394_new();

  err = dc1394_camera_enumerate (dc1394_, &list_);
  if (err) {
    perror("Camera enumerate");
    printf("Camera failed to enumerate firewire list: %d\n", err);
    return 1;
  }

  camera_ = dc1394_camera_new(dc1394_, list_->ids[address_].guid);

  err = dc1394_video_set_mode(camera_, DC1394_VIDEO_MODE_1024x768_MONO8);
  if (err) {
    perror("Camera video mode");
    printf("Camera failed to set video mode: %d\n", err);
  }

  err = dc1394_video_set_iso_speed(camera_, DC1394_ISO_SPEED_400);
  if (err) {
    perror("Camera iso speed");
    printf("Camera failed to set ISO speed to 400: %d\n", err);
    return 1;
  }

  // Can't go higher than 15 FPS given firewire bandwidth at max resolution
  err = dc1394_video_set_framerate(camera_, DC1394_FRAMERATE_15);
  if (err) {
    perror("Camera set framerate");
    printf("Camera failed to set framerate to 15: %d\n", err);
    return 1;
  }
  while (DMA_size_ >= 1 && (err = dc1394_capture_setup(camera_, DMA_size_, DC1394_CAPTURE_FLAGS_DEFAULT))) {
    --DMA_size_;
  }
  if (DMA_size_ < 1) {
    perror("Camera capture setup");
    printf("Camera failed to setup capture: %d\n", err);
    return 1;
  }
  printf("DMA size: %d\n", DMA_size_);


  if (camera_->one_shot_capable) {
    err = dc1394_video_set_one_shot(camera_, DC1394_ON);
    if (err) {
      perror("Camera one shot");
      printf("Camera failed to set one-shot mode");
      return 1;
    }
  } else {

    err = dc1394_video_set_transmission(camera_, DC1394_ON);

    if (err) {
      perror("Camera transmission");
      printf("Camera failed to start transmission: %d\n", err);
      return 1;
    }
  }

  err = dc1394_capture_dequeue(camera_, DC1394_CAPTURE_POLICY_WAIT, &frame_);
  if (err) {
    perror("Camera frame grab");
    printf("Camera failed to grab a single frame: %d\n", err);
  }

  //    dc1394_video_set_transmission(camera_, DC1394_OFF);
  do_bayer_ = (frame_->color_coding == DC1394_COLOR_CODING_MONO8 ||
               frame_->color_coding == DC1394_COLOR_CODING_MONO16 ||
               frame_->color_coding == DC1394_COLOR_CODING_MONO16S ||
               frame_->color_coding == DC1394_COLOR_CODING_RAW8 ||
               frame_->color_coding == DC1394_COLOR_CODING_RAW16);
  dc1394_capture_enqueue(camera_, frame_);

  err = dc1394_video_set_transmission(camera_, DC1394_ON);

  frame_ = (dc1394video_frame_t*)1;

  while (frame_) {
    err = dc1394_capture_dequeue(camera_, DC1394_CAPTURE_POLICY_POLL, &frame_);
    if (not err && frame_) {
      dc1394_capture_enqueue(camera_, frame_);
    }
  }
  int count = 0;
  struct timeval  tv;
  struct timespec ts;


  while (capture_running) {
    gettimeofday(&tv, NULL);
    long start_time_in_ms = (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000 ;

    frame_ = (dc1394video_frame_t*) 1;

    while (frame_) {
      err = dc1394_capture_dequeue(camera_, DC1394_CAPTURE_POLICY_POLL, &frame_);
      if (not err && frame_) {
        dc1394_capture_enqueue(camera_, frame_);
      }
    }

    err = dc1394_capture_dequeue(camera_, DC1394_CAPTURE_POLICY_WAIT, &frame_);
    gettimeofday(&tv, NULL);
    long image_acq_time = (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000 ;

    if (err || not frame_) {
      perror("Camera failed capture");
      printf("Camera failed to capture: %d\n", err);
      continue;
    }
    if (dc1394_capture_is_frame_corrupt(camera_, frame_)) {
      perror("Camera frame corrupt");
      printf("Camera received corrupt frame: %d\n", err);
      continue;
    }
    frame_->color_filter = DC1394_COLOR_FILTER_RGGB;
    unsigned char *data_ptr = NULL;
    if (do_bayer_) {
      dc1394video_frame_t* bayer_img = (i_buf % 2 ? &bayer_out0_ : &bayer_out1_);
      i_buf++;
      err = dc1394_debayer_frames(frame_, bayer_img, DC1394_BAYER_METHOD_SIMPLE);
      if (err != DC1394_SUCCESS) {
        perror("bayer error");
      }
      data_ptr = bayer_img->image;
    } else {
      data_ptr = frame_->image;
    }

    write_frame(framework, data_ptr, image_acq_time, width, height, 3);

    count++;
    gettimeofday(&tv, NULL);
    long sleep_time_in_miliseconds = (_MIN_IMAGE_TIME_IN_MS - ((tv.tv_sec) * 1000 + (tv.tv_usec) / 1000 - start_time_in_ms));
    if (sleep_time_in_miliseconds > 0) {
      ts.tv_sec = 0;
      ts.tv_nsec = sleep_time_in_miliseconds  * 1000000;
      nanosleep(&ts, NULL);
    }

    dc1394_capture_enqueue(camera_, frame_);
  }
  cleanup_message_framework(framework);
  return 0;
}
