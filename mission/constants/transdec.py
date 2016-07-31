from mission.constants.missions import Navigate, Recovery

PIPE_TO_BUOYS_DIST = 2.0
BUOY_BACKUP_DIST = 2.0
BUOY_BACKUP_FOR_OVER_DIST = 0.2
BUOY_OVER_DEPTH = 0.4
BUOY_SEARCH_DEPTH = 1.9
BUOY_TO_PIPE_DIST = 2.5

PIPE_SEARCH_DEPTH = 0.2
PIPE_FOLLOW_DEPTH = 0.5

BINS_CAM_TO_ARM_OFFSET = 0.12
BINS_HEIGHT = 0.35
#BINS_HEIGHT = 0.00
BINS_DROP_ALTITUDE = BINS_HEIGHT + 0.65
BINS_PICKUP_ALTITUDE = BINS_HEIGHT + 0.18
BINS_SEARCH_DEPTH = 0.5
#BINS_SEARCH_DEPTH = 0.1

HYDROPHONES_SEARCH_DEPTH = 0.5
HYDROPHONES_PINGER_DEPTH = 3.0

navigate = Navigate(
    max_distance=20,
    debugging=False,
    thresh_c=7,
    kernel_size=4,
    min_area=200,
    block_size=411,
)

recovery = Recovery(
    #stack_dive_altitude=1.75,
    stack_dive_altitude=1.8,
    vstack_grab_altitude=1.33,
    #hstack_grab_altitude=1.28,
    hstack_grab_altitude=1.3,
    #hstack_grab_altitude=1.4,

    debugging=False,
    table_c=-60,
    table_block_size=2000,
    table_min_area=20000,
    table_min_fill_ratio=0.8,
    red_stack_c=-20,
    green_stack_c=12,
    red_mark_c=-10,
    green_mark_c=10,
    min_stack_area=200,
    min_stack_width=30,
)
