import struct

import shm

from mission.framework.combinators import Sequential
from mission.framework.primitive import FunctionTask

def light_show(num):
    shm.leds.light_show.set(num)

def get_decimal_color(color):
    # It might be a name
    if color in colors:
        color = colors[color]

    i = int(color, 16)
    mask = int('FF', 16)

    return ((i >> 16) & mask, (i >> 8) & mask, i & mask)

def leds_color(port_color, star_color):
    leds = shm.leds.get()
    port = get_decimal_color(port_color)
    star = get_decimal_color(star_color)
    leds.port_color_red = port[0]
    leds.port_color_green = port[1]
    leds.port_color_blue = port[2]
    leds.starboard_color_red = star[0]
    leds.starboard_color_green = star[1]
    leds.starboard_color_blue = star[2]
    shm.leds.set(leds)

LightShow = lambda num: FunctionTask(lambda: light_show(num))

Leds = lambda port, star: FunctionTask(lambda: leds_color(port, star))
AllLeds = lambda color: Leds(color, color)

colors = {
    'black': '000000',
    'red': 'FF0000',
    'green': '00FF00',
    'blue': '0000FF',
    'yellow': 'FFFF00',
    'cyan': '00FFFF',
    'purple': 'FF00FF',
    'white': 'FFFFFF',
    'orange': 'FF3000',
    'pink': 'FF4545',
    'magenta': 'FF00FF',
}

InitLeds = Sequential(
    LightShow(1),
    AllLeds('magenta'),
)

TestBlack = AllLeds('black')
TestRed = AllLeds('red')
TestCyan = AllLeds('cyan')
TestOrange = AllLeds('orange')
TestWhite = AllLeds('white')
TestMagenta = AllLeds('magenta')

TestBothSides = Leds('red', 'blue')
