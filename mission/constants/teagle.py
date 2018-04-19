from mission.constants.missions import Wire, Recovery, ComplexColor, \
    RecoveryColor, Torpedoes, Buoys, Bins, BuoysVision, Gate

GATE_DISTANCE = 2
GATE_TIME = 3

PIPE_SEARCH_DEPTH = 0.4
PIPE_FOLLOW_DEPTH = 0.7


HYDROPHONES_PINGER_DEPTH = 3.0

bins = Bins(
    # Mission
    see_both_depth=0.5,
    above_bin_depth=2.2,
    grab_depth=3.2,

    # Vision
    border_thresh=135,
    border_bgr_compon=0,
    cover_thresh=50,
    clipping_guard=True,
)

buoys = Buoys(
    #mission
    BUOY_SEARCH_DEPTH = 1.8,
    BUOY_OVER_DEPTH = 1,
    BUOY_ORDER = ['G', 'Y', 'R'],
    BACKUP_TIME = 5,
    FORWARD_TIME = 2,

    #vision
    GREEN_REF = (100, 100, 150),
    YELLOW_REF = (180, 100, 180),
    RED_REF = (140, 150, 160),
    DOWNSCALE_RATIO = 0.33

)

wire = Wire(
    max_distance=4,
    debugging=False,
    thresh_c=8,
    kernel_size=4,
    min_area=200,
    block_size=411,

    DEFAULT_DEADBAND = 0.03,
    WIRE_DEPTH = 1.4,
    BBAR_RATIO_TARGET = .7,
    WIRE_THRU_DEPTH = 1.4
)

recovery = Recovery(
    tower_depth=0.5,
    tube_dive_altitude=1.8,
    tube_grab_altitude=1.26,
    table_depth=0.5,
    ellipse_depth=2.5,
    drop_depth=3.11,
    #high_drop_depth=2.9,

    detect_table=False,
    table_thresh=115,
    blue_ellipse_thresh=45,
    c0=-15,
    c1=-42,
    c2=255,

    # Tubes are attempted in this order
    colors=[
        RecoveryColor(
            name='red',
            tube_color=ComplexColor(lab_a=189, lab_b=165, ycrcb_cr=210, ycrcb_cb=99),
            ellipse_color=ComplexColor(lab_a=189, lab_b=165, ycrcb_cr=210, ycrcb_cb=99),
            size=1,
        ),
        RecoveryColor(
            name='green',
            tube_color=ComplexColor(lab_a=57, lab_b=191, ycrcb_cr=59, ycrcb_cb=73),
            ellipse_color=ComplexColor(lab_a=57, lab_b=191, ycrcb_cr=59, ycrcb_cb=73),
            size=1,
        ),
        RecoveryColor(
            name='blue',
            tube_color=ComplexColor(lab_a=178, lab_b=49, ycrcb_cr=115, ycrcb_cb=208),
            ellipse_color=ComplexColor(lab_a=178, lab_b=49, ycrcb_cr=115, ycrcb_cb=208),
            size=2/3,
        ),
        # Same color as red to force classification by size
        RecoveryColor(
            name='orange',
            tube_color=ComplexColor(lab_a=189, lab_b=165, ycrcb_cr=210, ycrcb_cb=99),
            ellipse_color=ComplexColor(lab_a=189, lab_b=165, ycrcb_cr=210, ycrcb_cb=99),
            size=2/3,
        ),
    ]
)

torpedoes = Torpedoes(
    depth=2,
)

buoysVision = BuoysVision(
    #o god so many im sorry
    hls_l_min = 0,
    hls_l_max = 255,
    red_hls_h_min = 0,
    red_hls_h_max = 50,
    red_ycrcb_cb_min = 0,
    red_ycrcb_cb_max = 115,
    red_lab_a_min = 130,
    red_lab_a_max = 255,
    red_lab_b_min = 145,
    red_lab_b_max = 230,
    red_erode_kernel_size = 1,
    red_dilate_kernel_size = 1,
    green_lab_l_min = 80,
    green_lab_l_max = 141,
    green_hsv_v_min = 29,
    green_hsv_v_max = 147,
    green_hsv_h_min = 28,
    green_hsv_h_max = 66,
    green_ycrcb_y_min = 16,
    green_ycrcb_y_max = 121,
    green_erode_kernel_size = 4,
    green_dilate_kernel_size = 3,
    yellow_hls_h_min = 17,
    yellow_hls_h_max = 49,
    yellow_ycrcb_cb_min = 0,
    yellow_ycrcb_cb_max = 111,
    yellow_lab_a_min = 0,
    yellow_lab_a_max = 200,
    yellow_lab_b_min = 154,
    yellow_lab_b_max = 255,
    yellow_erode_kernel_size = 1,
    yellow_dilate_kernel_size = 1,
    blur_kernel = 1,
    max_error = 5000,
    max_buoy_error = 1500,
    min_circularity = 0.62,
    min_percent_frame = 0.001,
    ex = 243,
    ey = 171,
    max_y_diff = 150,
    min_x_diff=50,
)

gate = Gate(
    depth=0.5,
    distance=2,
    vel_time=3,
    artemis_vel_time=10,
)
