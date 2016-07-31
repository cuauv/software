import time

from termcolor import colored


ts = lambda: grey(time.strftime('%m/%d/%Y %H:%M:%S %Z'))

grey = lambda msg: colored(msg, 'grey')
red = lambda msg: colored(msg, 'red')
green = lambda msg: colored(msg, 'green')
yellow = lambda msg: colored(msg, 'yellow')
blue = lambda msg: colored(msg, 'blue')
magenta = lambda msg: colored(msg, 'magenta')
cyan = lambda msg: colored(msg, 'cyan')
white = lambda msg: colored(msg, 'white')