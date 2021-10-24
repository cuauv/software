import numpy as np

BIT_DEPTH = 16384
BOARD_ADDR = '192.168.93.102'
CLIPPING_THRESHOLD = 8000
GAIN_VALUES = [1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128]
GUI_UPDATE_TIME = 0.1
L_GAIN_PLOT = 70
L_PKT = 62
NIPPLE_DIST = 0.01778
NUM_CHS = 4
PKTS_PER_RECV = 256
SAMPLE_RATE = 153061
SOUND_SPEED = 1481
USE_4CHS = False

SAMPLE_PKT_DTYPE = np.dtype([
    ('pkt_num', '<i4'),
    ('samples', str((NUM_CHS, L_PKT)) + '<i2'),
    ('max_sample', '<i2'),
    ('pkt_type', '<i1'),
    ('gain_lvl', '<i1')
])

CONF_PKT_DTYPE = np.dtype([
    ('reset', '?'),
    ('autogain', '?'),
    ('man_gain_lvl', '<i1')
])