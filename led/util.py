import shm

'''
# Previous code for 60-LED array

NUM_LEDS = 60

leds = [shm._eval('led.val_{}'.format(i)) for i in range(NUM_LEDS)]

def rgb2int(r, g, b):
  return (r << 5) + (g << 2) + b

def int2rgb(val):
  red = val >> 5
  green = (val & 0x1c) >> 2
  blue = (val & 0x03)
  return (red, green, blue)

def roundoff(r, g, b):
  return (int(round(7 * r)), int(round(7 * g)), int(round(3 * b)))

def unroundoff(r, g, b):
  return (r / 7., g / 7., b / 3.)

# Get the current (r, g, b) value a particular LED is set to.
def get_one(ind):
  return unroundoff(*int2rgb(leds[ind].get()))

# Set a particular LED to a particular (r, g, b) value.
def set_one(ind, val):
  leds[ind].set(rgb2int(*roundoff(*val)))

# Set all LEDs to a particular (r, g, b) value.
def set_all(val):
  grp = shm.led.get() 
  val = rgb2int(*roundoff(*val))
  for ind in range(NUM_LEDS):
    exec('grp.val_{} = val'.format(ind))
  shm.led.set(grp)
'''

led = shm.led.val

def rgb2int(r, g, b):
    return (r << 11) + (g << 6) + (b << 1)

def int2rgb(val):
    red = val >> 11
    green = (val & 0x07C0) >> 6
    blue = (val & 0x3E) >> 1
    return (red, green, blue)

def roundoff(r, g, b):
    return (int(round(31 * r)), int(round(31 * g)), int(round(31 * b)))

def set(val):
    led.set(rgb2int(*roundoff(*val)))

class colors:
  white   = (1.0, 1.0, 1.0)
  red     = (1.0, 0.0, 0.0)
  green   = (0.0, 1.0, 0.0)
  blue    = (0.0, 0.0, 1.0)
  yellow  = (1.0, 1.0, 0.0)
  none    = (0.0, 0.0, 0.0)

def rainbow():
  import colorsys
  import time

  hue = 0
  while 1:
    color = colorsys.hsv_to_rgb(hue, 1.0, 1.0)
    for i in range(60):
      color = colorsys.hsv_to_rgb(hue + 0.03 * i, 1.0, 1.0)
      set(color)
      time.sleep(0.01)

    hue += 0.01
    time.sleep(0.01)
