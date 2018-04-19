#ifndef __FIND_POWERMATE__DOT_H__SENTRY__
#define __FIND_POWERMATE__DOT_H__SENTRY__

int open_powermate(const char *dev, int mode);
int find_powermate(int mode);
void powermate_pulse_led(int fd, int static_brightness, int pulse_speed, int pulse_table, int pulse_asleep, int pulse_awake);

#ifndef MSC_PULSELED
/* this may not have made its way into the kernel headers yet ... */
#define MSC_PULSELED 0x01
#endif

#endif
