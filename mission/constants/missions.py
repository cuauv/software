from collections import namedtuple

Bins = namedtuple('Bins', [
    # Mission
    'see_both_depth',
    'above_bin_depth',
    'grab_depth',

    # Vision
    'border_thresh',
    'border_bgr_compon',
    'cover_thresh',
    'clipping_guard',
])

Buoys = namedtuple('Buoys', [
    'BUOY_SEARCH_DEPTH',
    'BUOY_OVER_DEPTH',
    'BUOY_ORDER',
    'GREEN_REF',
    'YELLOW_REF',
    'RED_REF',
    'DOWNSCALE_RATIO',
    'FORWARD_TIME',
    'BACKUP_TIME'
])

Wire = namedtuple('Wire', [
    'max_distance',
    'debugging',
    'thresh_c',
    'kernel_size',
    'min_area',
    'block_size',
    'DEFAULT_DEADBAND',
    'WIRE_DEPTH',
    'BBAR_RATIO_TARGET',
    'WIRE_THRU_DEPTH',
])

Recovery = namedtuple('Recovery', [
    'tower_depth',
    'tube_dive_altitude',
    'tube_grab_altitude',
    'table_depth',
    'ellipse_depth',
    'drop_depth',

    'detect_table',
    'table_thresh',
    'blue_ellipse_thresh',
    'c0',
    'c1',
    'c2',

    'colors',
])

ComplexColor = namedtuple('ComplexColor', [
    'lab_a',
    'lab_b',
    'ycrcb_cr',
    'ycrcb_cb',
])

RecoveryColor = namedtuple('RecoveryColor', [
    'name',
    'tube_color',
    'ellipse_color',
    'size',
])

Torpedoes = namedtuple('Torpedoes', [
    'depth', # Depth of the center of the boards
])

BuoysVision = namedtuple('BuoysVision', [
    #o god so many im sorry
    'hls_l_min',
    'hls_l_max',
    'red_hls_h_min',
    'red_hls_h_max',
    'red_ycrcb_cb_min',
    'red_ycrcb_cb_max',
    'red_lab_a_min',
    'red_lab_a_max',
    'red_lab_b_min',
    'red_lab_b_max',
    'red_erode_kernel_size',
    'red_dilate_kernel_size',
    'green_lab_l_min',
    'green_lab_l_max',
    'green_hsv_v_min',
    'green_hsv_v_max',
    'green_hsv_h_min',
    'green_hsv_h_max',
    'green_ycrcb_y_min',
    'green_ycrcb_y_max',
    'green_erode_kernel_size',
    'green_dilate_kernel_size',
    'yellow_hls_h_min',
    'yellow_hls_h_max',
    'yellow_ycrcb_cb_min',
    'yellow_ycrcb_cb_max',
    'yellow_lab_a_min',
    'yellow_lab_a_max',
    'yellow_lab_b_min',
    'yellow_lab_b_max',
    'yellow_erode_kernel_size',
    'yellow_dilate_kernel_size',
    'blur_kernel',
    'max_error',
    'max_buoy_error',
    'min_circularity',
    'min_percent_frame',
    'ex',
    'ey',
    'max_y_diff',
    'min_x_diff',
])

Gate = namedtuple('Gate', ['depth', 'distance', 'vel_time', 'artemis_vel_time'])
