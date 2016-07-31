#include <linux/input.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "findpowermate.h"

#define NUM_EVENT_DEVICES 16

int open_powermate(const char *dev, int mode)
{
  int fd = open(dev, mode);
  char name[255];
  const char *valid_id_prefix = "Griffin";

  if(fd < 0){
    fprintf(stderr, "Unable to open \"%s\": %s\n", dev, strerror(errno));
    return -1;
  }

  if(ioctl(fd, EVIOCGNAME(sizeof(name)), name) < 0){
    fprintf(stderr, "\"%s\": EVIOCGNAME failed: %s\n", dev, strerror(errno));
    close(fd);
    return -1;
  }

  // it's the correct device if the prefix matches what we expect it to be:
  if(!strncasecmp(name, valid_id_prefix, strlen(valid_id_prefix)))
    return fd;

  close(fd);
  return -1;
}

int find_powermate(int mode)
{
  char devname[256];
  int i, r;

  for(i=0; i<NUM_EVENT_DEVICES; i++){
    sprintf(devname, "/dev/input/event%d", i);
    r = open_powermate(devname, mode);
    if(r >= 0)
      return r;
  }

  return -1;
}

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
