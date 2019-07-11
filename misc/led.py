#!/usr/bin/env python3

import colorsys
import itertools
import multiprocessing
import sys
import time

import numpy as np
import shm

from utils import register_exit_signals, watch_thread_wrapper

########  Constants  ########


LEDS = {
    'port': {
        'red': shm.leds_internal.port_color_red,
        'green': shm.leds_internal.port_color_green,
        'blue': shm.leds_internal.port_color_blue,
    },
    'starboard': {
        'red': shm.leds_internal.starboard_color_red,
        'green': shm.leds_internal.starboard_color_green,
        'blue': shm.leds_internal.starboard_color_blue,
    },
}


COLORS = {
    "WHITE": (255, 255, 255),
    "RED": (255, 0, 0),
    "BLUE": (0, 0, 255),
    "GREEN": (0, 255, 0),
    "ORANGE": (255, 165, 0),
    "CYAN": (0, 255, 255),
    "MAGENTA": (255, 0, 255),
    "YELLOW": (255, 255, 0),
    "BLACK": (0, 0, 0),
}

LIGHTSHOWS = {
    'none': 0,
    'color': 1,
    'unkilled': 2,
    'killed': 3,
    'torpedo': 4,
    'mrinal': 5,
    'pulse': 7,
    'rainbow': 8,
    'enable_pulse': 9,
}

class lightshow_helper:
    def __init__(self, lightshow, func):
        self.lightshow = LIGHTSHOWS[lightshow]
        self.func = func
    def init(self):
        shm.leds_internal.light_show.set(self.lightshow)
    def __call__(self, *args, **kwargs):
        self.func(*args, **kwargs)


def lightshow(name):
    #n = LIGHTSHOWS[name]
    def decorator(f):
        return lightshow_helper(name, f)
        #class _wrap(*args, **kwargs):
        #    shm.leds_internal.light_show.set(n)
        #    return f(*args, **kwargs)
        #return _wrap
    return decorator

#######  Helpers  ######


def hsv_to_rgb(h, s, v):
    return tuple(map(lambda c: int(255 * c), colorsys.hsv_to_rgb(h, s, v)))


def interp_color(v, xs, ys, left=(None, None, None), right=(None, None, None)):
    """
    v - Value to interpolate
    xs - X values to interpolate between
    ys - Color outputs that correspond to xs
    left - Optional clipping value when too low
    right - Optional clipping value when too high
    """

    return tuple(map(lambda args: int(np.interp(*args)), zip((v, v, v), (xs, xs, xs), zip(*ys), left, right)))


def set_side_rgb(side, r, g, b):
    side["red"].set(r)
    side["green"].set(g)
    side["blue"].set(b)


def set_side_hsv(side, h, s, v):
    set_side_rgb(side, *hsv_to_rgb(h, s, v))


def set_side(side, v):
    set_side_rgb(side, v, v, v)


def set_all_rgb(r, g, b):
    set_side_rgb(LEDS["port"], r, g, b)
    set_side_rgb(LEDS["starboard"], r, g, b)


def set_all_hsv(h, s, v):
    set_all_rgb(*hsv_to_rgb(h, s, v))


def set_all(v):
    set_all_rgb(v, v, v)


#######  Modes  #######


@lightshow('color')
def passthrough():
    leds = shm.leds.get()
    leds_internal = shm.leds_internal.get()

    leds_internal.port_color_red = leds.port_color_red % 256
    leds_internal.port_color_green = leds.port_color_green % 256
    leds_internal.port_color_blue = leds.port_color_blue % 256
    leds_internal.starboard_color_red = leds.starboard_color_red % 256
    leds_internal.starboard_color_green = leds.starboard_color_green % 256
    leds_internal.starboard_color_blue = leds.starboard_color_blue % 256

    shm.leds_internal.set(leds_internal)


@lightshow('color')
def blink_mode():
    while True:
        set_all(255)
        time.sleep(0.1)
        set_all(0)
        time.sleep(0.1)


@lightshow('color')
def rainbow():
    period = 5

    hue_port = (time.time() / period) % 1
    hue_star = (time.time() / period + period / 2) % 1

    for side, hue in zip(LEDS.values(), (hue_port, hue_star)):
        set_side_hsv(side, hue, 1, 1)

def blink(pins, c, Ton, Toff, val=255):
    for i in range(c):
        for pin in pins: pin.set(val)
        time.sleep(Ton)
        for pin in pins: pin.set(0)
        time.sleep(Toff)
@lightshow('color')
def police():
    int3 = 0.07
    for i in LEDS.values():
        for j in i.values():
            j.set(0)
    LED1 = LEDS['port']['red']
    LED2 = LEDS['starboard']['blue']
    LEDS_USED = [LED1, LED2]
    def quint():
        blink(LEDS_USED, 5)

    while True:
        for j in range(5):
            for i in range(2):
                blink([LED1],4, int3, .03)
                blink([LED2],4, int3, .03)
            for i in range(8):
                blink([LED1],1, int3, .03)
                blink([LED2],1, int3, .03)
        for j in range(3):
            blink(LEDS_USED, 5, int3, .03)
            time.sleep(int3 * 4)


@lightshow('color')
def pressure():
    set_all_rgb(
        *interp_color(
            shm.pressure.hull.get(),
            [0.7, 0.735, .9],
            [COLORS["GREEN"], COLORS["GREEN"], COLORS["ORANGE"]],
            left=COLORS["RED"],
            right=COLORS["RED"],
        )
    )

@lightshow('color')
def voltage_helper(v, side):
    if v < 1:
        set_side(side, 0)
    elif v < 13.0:
        set_side_rgb(side, *COLORS["RED"])
        time.sleep(0.2)
        set_side_rgb(side, 0, 0, 0)
        time.sleep(0.1)
    else:
        set_side_rgb(side, 
            *interp_color(
                v,
                [13.0, 14.2, 15],
                [COLORS["RED"], COLORS["YELLOW"], COLORS["GREEN"]],
                right=COLORS["GREEN"]
            )
        )
@lightshow('color')
def voltage_side():
    voltage_helper(shm.merge_status.voltage_a.get(), LEDS["port"])
    voltage_helper(shm.merge_status.voltage_b.get(), LEDS["starboard"])
@lightshow('color')
def voltage():
    v = shm.merge_status.total_voltage.get()
    voltage_helper(v, LEDS["port"])
    voltage_helper(v, LEDS["starboard"])

last_time = time.time()

@lightshow('color')
def trim():
    bad_angle = 7
    mid_angle = 3
    good_angle = 1

    set_side_rgb(
        LEDS["port"],
        *interp_color(
            shm.gx4.roll.get(),
            [-bad_angle, -mid_angle, -good_angle, good_angle, mid_angle, bad_angle],
            [COLORS["RED"], COLORS["ORANGE"], COLORS["GREEN"], COLORS["GREEN"], COLORS["ORANGE"], COLORS["RED"]],
            left=COLORS["RED"],
            right=COLORS["RED"],
        )
    )

    set_side_rgb(
        LEDS["starboard"],
        *interp_color(
            shm.gx4.pitch.get(),
            [-bad_angle, -mid_angle, -good_angle, good_angle, mid_angle, bad_angle],
            [COLORS["RED"], COLORS["ORANGE"], COLORS["GREEN"], COLORS["GREEN"], COLORS["ORANGE"], COLORS["RED"]],
            left=COLORS["RED"],
            right=COLORS["RED"],
        )
    )

@lightshow('color')
def feedback():
    leds = shm.leds.get()
    leds_internal = shm.leds_internal.get()

    leds_internal.port_color_red = leds.port_color_red % 256
    leds_internal.port_color_green = leds.port_color_green % 256
    leds_internal.port_color_blue = leds.port_color_blue % 256
    leds_internal.starboard_color_red = leds.port_color_red % 256
    leds_internal.starboard_color_green = leds.port_color_green % 256
    leds_internal.starboard_color_blue = leds.port_color_blue % 256

    shm.leds_internal.set(leds_internal)


@lightshow('color')
def feedback_pulse():
    for i in itertools.cycle(range(15)):
        leds = shm.leds.get()
        leds_internal = shm.leds_internal.get()


        if 0 <= i < 12:
            leds_internal.port_color_red = leds.port_color_red % 256
            leds_internal.port_color_green = leds.port_color_green % 256
            leds_internal.port_color_blue = leds.port_color_blue % 256
            leds_internal.starboard_color_red = leds.port_color_red % 256
            leds_internal.starboard_color_green = leds.port_color_green % 256
            leds_internal.starboard_color_blue = leds.port_color_blue % 256
        else:
            leds_internal.port_color_red = leds.starboard_color_red % 256
            leds_internal.port_color_green = leds.starboard_color_green % 256
            leds_internal.port_color_blue = leds.starboard_color_blue % 256
            leds_internal.starboard_color_red = leds.starboard_color_red % 256
            leds_internal.starboard_color_green = leds.starboard_color_green % 256
            leds_internal.starboard_color_blue = leds.starboard_color_blue % 256

        shm.leds_internal.set(leds_internal)

        time.sleep(0.1)


@lightshow('color')
def feedback_pulse2():
    for i in itertools.cycle(range(21)):
        leds = shm.leds.get()
        leds_internal = shm.leds_internal.get()


        if 0 <= i < 12 or 15 <= i < 18:
            leds_internal.port_color_red = leds.port_color_red % 256
            leds_internal.port_color_green = leds.port_color_green % 256
            leds_internal.port_color_blue = leds.port_color_blue % 256
            leds_internal.starboard_color_red = leds.port_color_red % 256
            leds_internal.starboard_color_green = leds.port_color_green % 256
            leds_internal.starboard_color_blue = leds.port_color_blue % 256
        else:
            leds_internal.port_color_red = leds.starboard_color_red % 256
            leds_internal.port_color_green = leds.starboard_color_green % 256
            leds_internal.port_color_blue = leds.starboard_color_blue % 256
            leds_internal.starboard_color_red = leds.starboard_color_red % 256
            leds_internal.starboard_color_green = leds.starboard_color_green % 256
            leds_internal.starboard_color_blue = leds.starboard_color_blue % 256

        shm.leds_internal.set(leds_internal)

        time.sleep(0.1)


@lightshow('color')
def morse():
    TIMES = {
        ".": 0.1,
        "-": 0.2,
        " ": 0.2,
        "": 0.1,
        "/": 0.5,
    }

    full_stop = 0.5

    MORSE_CODE_DICT = {
        'A':'.-', 'B':'-...',
        'C':'-.-.', 'D':'-..', 'E':'.',
        'F':'..-.', 'G':'--.', 'H':'....',
        'I':'..', 'J':'.---', 'K':'-.-',
        'L':'.-..', 'M':'--', 'N':'-.',
        'O':'---', 'P':'.--.', 'Q':'--.-',
        'R':'.-.', 'S':'...', 'T':'-',
        'U':'..-', 'V':'...-', 'W':'.--',
        'X':'-..-', 'Y':'-.--', 'Z':'--..',
        '1':'.----', '2':'..---', '3':'...--',
        '4':'....-', '5':'.....', '6':'-....',
        '7':'--...', '8':'---..', '9':'----.',
        '0':'-----', ', ':'--..--', '.':'.-.-.-',
        '?':'..--..', '/':'-..-.', '-':'-....-',
        '(':'-.--.', ')':'-.--.-', ' ': ' ',
    }

    while True:
        message = shm.lcd.message.get()

        for c in message:
            print(c)

            code = MORSE_CODE_DICT[c.upper()]
            for sym in code:
                set_all(255)
                time.sleep(TIMES[sym])
                set_all(0)
                time.sleep(TIMES[""])

        time.sleep(TIMES["/"])


@lightshow('color')
def on():
    set_all(255)


@lightshow('color')
def off():
    set_all(0)

def internal_lightshow(n):
    return lightshow(n)(lambda: None)
#@lightshow('rainbow')
#def rainbow2():
#    pass


modes = {
    "blink": blink_mode,
    "passthrough": passthrough,
    "off": off,
    "on": on,
    "rainbow": rainbow,
    "rainbow2": internal_lightshow('rainbow'),
    "pulse": internal_lightshow('pulse'),
    "enable_pulse": internal_lightshow('enable_pulse'),
    "pressure": pressure,
    "voltage": voltage,
    "voltage_side": voltage_side,
    "police": police,
    "trim": trim,
    "feedback": feedback,
    "feedback_pulse": feedback_pulse,
    "feedback_pulse2": feedback_pulse2,
    "morse": morse,
}


#######  LED Runner internal  ########


def noop(_, __):
    sys.exit(0)


def run_loop(func):
    register_exit_signals(noop)
    if hasattr(func, 'init'): func.init()
    while True:
        func()
        time.sleep(0.1)


def control_loop(watcher, quit_event):
    watcher.watch(shm.leds)

    last_mode = "--"
    led_process = None

    while not quit_event.is_set():
        modename = shm.leds.mode.get()

        if modename != last_mode:
            last_mode = modename
            if led_process is not None:
                led_process.terminate()
            led_controller_func = modes.get(modename, passthrough)
            led_process = multiprocessing.Process(target=run_loop, args=(led_controller_func,), daemon=True)
            led_process.start()

        watcher.wait(new_update=False)


watch_thread_wrapper(control_loop)
