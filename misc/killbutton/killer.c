#include <linux/input.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "findpowermate.h"
#include "killsubmarine.hpp"

int abs_offset = 0;

int counter = 0;

int powermate = -1;

int spin_thresh = 6;

void process_event(struct input_event *ev)
{
#ifdef VERBOSE
  fprintf(stderr, "type=0x%04x, code=0x%04x, value=%d\n",
	  ev->type, ev->code, (int)ev->value);
#endif

  //kill the sub
    if(ev->code == BTN_0){
        if (counter % 2 == 0){
            #ifdef VERBOSE
            printf("Sending Kill Signal...\n");
            printf("...\n");
            printf("KILLED!\n");
            #endif
            KillSubmarine();
            //powermate_pulse_led(int fd, int static_brightness, int pulse_speed, int pulse_table, int pulse_asleep, int pulse_awake)
            powermate_pulse_led(powermate, 0, 255, 2, 0, 0);
        }
        counter++;
    }
    else if(ev->code == REL_DIAL){
        if((int)ev->value > spin_thresh || (int)ev->value < -1*spin_thresh)
        {
            #ifdef VERBOSE
            printf("Sending UNKill Signal...\n");
            printf("...\n");
            printf("ALIVE!\n");
            #endif
            UnkillSubmarine();
            //Heart beat pulse
            powermate_pulse_led(powermate, 0, 255, 2, 1, 1);
        }
        
    }

  fflush(stdout);
}
char process_button_press(struct input_event *ev)
{
#ifdef VERBOSE
  fprintf(stderr, "type=0x%04x, code=0x%04x, value=%d\n",
	  ev->type, ev->code, (int)ev->value);
#endif

    if(ev->code == BTN_0)
        return '1';
    else
        return '0';

 fflush(stdout);
}

#define BUFFER_SIZE 32
void watch_powermate(int fd)
{
  struct input_event ibuffer[BUFFER_SIZE];
  int r, events, i;

  while(1){
    r = read(fd, ibuffer, sizeof(struct input_event) * BUFFER_SIZE);
    if( r > 0 ){
      events = r / sizeof(struct input_event);
      for(i=0; i<events; i++)
	    process_event(&ibuffer[i]);
    }else{
      fprintf(stderr, "read() failed: %s\n", strerror(errno));
      return;
    }
  }
}

int get_event_from_powermate(int fd)
{
  struct input_event ibuffer[BUFFER_SIZE];
  int r, events, i;

  while(1){
    r = read(fd, ibuffer, sizeof(struct input_event) * BUFFER_SIZE);
    if( r > 0 ){
      events = r / sizeof(struct input_event);
      for(i=0; i<events; i++){ 
        char bval = process_button_press(&ibuffer[i]);
        return bval;
        }
    }else{
      fprintf(stderr, "read() failed: %s\n", strerror(errno));
      return 0;
    }
  }
}

int main(int argc, char *argv[])
{
  if(argc == 1)
    powermate = find_powermate(O_RDWR);
  else
    powermate = open_powermate(argv[1], O_RDWR);

  if(powermate < 0){
    fprintf(stderr, "Unable to locate powermate\n");
    fprintf(stderr, "Try: %s [device]\n", argv[0]);
    return 1;
  }

  watch_powermate(powermate);
  //get_event_from_powermate(powermate);   

  close(powermate);

  return 0;
}
