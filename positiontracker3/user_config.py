# Config file for user settings
# Change anything as desired

''' Navigation Settings '''
PAN_DIST = 15
PAN_DIST_LARGE = 60
ZOOM_FACTOR = 1.35


'''Canvas Settings'''
SUB_COLOR = (1, 0, 0, 1)
PATH_COLOR = (0, 0, 0, 0.7)
GRID_COLOR = (0.4, 0, 0, 0.3)
TAG_COLOR = (0, 0, 1, 0.8)
ELEMENT_COLOR = (0, .5, 0, 1.0)
MAGNIFY = 40        #How many pixels correspond to 1 unit in shm
PATH_RES = 0.15     #Path resolution
SUB_SIZE = 8
TAG_SIZE = 5
PATH_LEN = -1   #neg. number indicates unbounded

GRID_LINES = 50
GRID_SPACE = 1. #in meters

SMOOTHING = True
PERIOD = .01   # Period of smooth updates, smaller <=> faster smoothing


''' Key bindings '''
# You can bind the control key with the format 'ctrl [key]'
# The same applies to the shift key: 'shift [key]'
# NOTE: Do not use uppercase letters, use shift + letter
# To include both control and shift, ctrl comes first: 'ctrl shift [key]'
bindings_on = True

key_binds = {}
key_binds["quit"] = "shift q"
key_binds["bindings toggle"] = "shift b"
key_binds["help"] = "h"

key_binds["follow sub"] = "shift f"
key_binds["follow position only"] = "f"
key_binds["reset path"] = "r"

key_binds["zoom in"] = "i"
key_binds["zoom out"] = "o"

key_binds["pan left"] = "left"
key_binds["pan left large"] = "shift left"
key_binds["pan right"] = "right"
key_binds["pan right large"] = "shift right"
key_binds["pan up"] = "up"
key_binds["pan up large"] = "shift up"
key_binds["pan down"] = "down"
key_binds["pan down large"] = "shift down"
key_binds["center view"] = "c"

key_binds["rotate cw"] = "ctrl left"
key_binds["rotate ccw"] = "ctrl right"
