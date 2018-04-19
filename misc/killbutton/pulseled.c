#include <linux/input.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include "findpowermate.h"

#ifndef MSC_PULSELED
/* this may not have made its way into the kernel headers yet ... */
#define MSC_PULSELED 0x01
#endif

void powermate_pulse_led(int fd, int static_brightness, int pulse_speed, int pulse_table, int pulse_asleep, int pulse_awake)
{
  struct input_event ev;
  memset(&ev, 0, sizeof(struct input_event));
  
  static_brightness &= 0xFF;

  if(pulse_speed < 0)
    pulse_speed = 0;
  if(pulse_speed > 510)
    pulse_speed = 510;
  if(pulse_table < 0)
    pulse_table = 0;
  if(pulse_table > 2)
    pulse_table = 2;
  pulse_asleep = !!pulse_asleep;
  pulse_awake = !!pulse_awake;

  ev.type = EV_MSC;
  ev.code = MSC_PULSELED;
  ev.value = static_brightness | (pulse_speed << 8) | (pulse_table << 17) | (pulse_asleep << 19) | (pulse_awake << 20);

  if(write(fd, &ev, sizeof(struct input_event)) != sizeof(struct input_event))
    fprintf(stderr, "write(): %s\n", strerror(errno));  
}

void usage(void)
{
  printf("Usage:\n");
  printf("pulseled [options]\n");
  printf("\t--device [filename]\tEvent device to use\n");
  printf("\t--level [level]    \tLED intensity with pulsing disabled (0-255)\n");
  printf("\t--speed [rate]     \tPulsing speed (0-510); around 255 works best\n");
  printf("\t--style [style]    \tPulsing style (0, 1 or 2)\n");
  printf("\t--pulseawake       \tEnable pulsing whilst host is awake\n");
  printf("\t--nopulseawake     \tDisable pulsing whilst host is awake\n");
  printf("\t--pulseasleep      \tEnable pulsing whilst host is asleep\n");
  printf("\t--nopulseasleep    \tDisable pulsing whilst host is asleep\n");
}

#define TAKE_NEXT_ARG {if(++args_processed >= argc){fprintf(stderr, "Missing argument to %s!\n", argv[args_processed-1]);return 1;}}

int main(int argc, char *argv[])
{
  int powermate = -1;
  int args_processed = 1;
  int static_brightness = 0x80;
  int pulse_speed = 255;
  int pulse_table = 0;
  int pulse_asleep = 1;
  int pulse_awake = 0;

  const char *device = 0;

  while(args_processed < argc){
    if(!strcasecmp(argv[args_processed], "--device")){
      TAKE_NEXT_ARG;
      device = argv[args_processed];
    }else if(!strcasecmp(argv[args_processed], "--level")){
      TAKE_NEXT_ARG;
      static_brightness = atoi(argv[args_processed]);
    }else if(!strcasecmp(argv[args_processed], "--speed")){
      TAKE_NEXT_ARG;
      pulse_speed = atoi(argv[args_processed]);
    }else if(!strcasecmp(argv[args_processed], "--style")){
      TAKE_NEXT_ARG;
      pulse_table = atoi(argv[args_processed]);
    }else if(!strcasecmp(argv[args_processed], "--pulseawake")){
      pulse_awake = 1;
    }else if(!strcasecmp(argv[args_processed], "--pulseasleep")){
      pulse_asleep = 1;
    }else if(!strcasecmp(argv[args_processed], "--nopulseawake")){
      pulse_awake = 0;
    }else if(!strcasecmp(argv[args_processed], "--nopulseasleep")){
      pulse_asleep = 0;
    }else{
      usage();
      return 1;
    }

    args_processed++;
  }

  if(device)
    powermate = open_powermate(device, O_WRONLY);
  else
    powermate = find_powermate(O_WRONLY);

  if(powermate < 0){
    fprintf(stderr, "Unable to locate powermate\n");
    fprintf(stderr, "Try: %s [device]\n", argv[0]);
    return 1;
  }

  powermate_pulse_led(powermate, static_brightness, pulse_speed, pulse_table, pulse_asleep, pulse_awake);
  
  close(powermate);

  return 0;
}
