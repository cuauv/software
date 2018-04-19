from mission.constants.missions import Wire, Recovery, ComplexColor, \
    RecoveryColor, Torpedoes, Buoys, Bins, BuoysVision, Gate

PIPE_SEARCH_DEPTH = 0.8
PIPE_FOLLOW_DEPTH = 1
PIPE_RIGHT_FIRST = False

HYDROPHONES_PINGER_DEPTH = 4.7

bins = Bins(
    # Mission
    see_both_depth=0.8,
    above_bin_depth=2.6,
    grab_depth=3.65,

    # Vision
    border_thresh=220,
    border_bgr_compon=0,
    cover_thresh=122,
    clipping_guard=False,
)

buoys = Buoys(
    #mission
    BUOY_SEARCH_DEPTH = 1.7,
    BUOY_OVER_DEPTH = 1,
    BUOY_ORDER = ['Y', 'R', 'G'],
    BACKUP_TIME = 5,
    FORWARD_TIME = 2,

    #vision
    GREEN_REF = (210, 90, 150),
    YELLOW_REF = (200, 120, 160),
    RED_REF = (180, 150, 150),
    DOWNSCALE_RATIO = 0.33
)

wire = Wire(
    max_distance=4,
    debugging=False,
    thresh_c=10,
    kernel_size=4,
    min_area=200,
    block_size=411,

    DEFAULT_DEADBAND = 0.03,
    WIRE_DEPTH = 1.85,
    BBAR_RATIO_TARGET = .8,
    WIRE_THRU_DEPTH = 1.8
)

recovery = Recovery(
    tower_depth=1,
    tube_dive_altitude=2,
    tube_grab_altitude=1.50, # Measured value for 1" above is 1.5, so 1.26 is reasonable
    table_depth=1,
    ellipse_depth=3.2,
    drop_depth=3.4,

    detect_table=True,
    table_thresh=105,
    blue_ellipse_thresh=43,
    c0=-20,
    c1=12,
    c2=21,

    colors=[
        # Same color as red to force classification by size
        RecoveryColor(
            name='red',
            tube_color=ComplexColor(lab_a=138, lab_b=129, ycrcb_cr=142, ycrcb_cb=127),
            ellipse_color=ComplexColor(lab_a=138, lab_b=129, ycrcb_cr=142, ycrcb_cb=127),
            size=1,
        ),
        RecoveryColor(
           name='green',
           tube_color=ComplexColor(lab_a=98, lab_b=138, ycrcb_cr=96, ycrcb_cb=124),
           ellipse_color=ComplexColor(lab_a=94, lab_b=142, ycrcb_cr=92, ycrcb_cb=120),
           size=1,
        ),
        RecoveryColor(
            name='orange',
            tube_color=ComplexColor(lab_a=138, lab_b=129, ycrcb_cr=142, ycrcb_cb=127),
            ellipse_color=ComplexColor(lab_a=138, lab_b=129, ycrcb_cr=142, ycrcb_cb=127),
            size=2/3,
        ),
        RecoveryColor(
            name='blue',
            tube_color=ComplexColor(lab_a=125, lab_b=99, ycrcb_cr=93, ycrcb_cb=159),
            ellipse_color=ComplexColor(lab_a=133, lab_b=87, ycrcb_cr=96, ycrcb_cb=169),
            size=2/3,
        ),
        # Original, red/oranage not averaged
        #RecoveryColor(
        #    name='red',
        #    tube_color=ComplexColor(lab_a=156, lab_b=131, ycrcb_cr=156, ycrcb_cb=126),
        #    ellipse_color=ComplexColor(lab_a=136, lab_b=117, ycrcb_cr=131, ycrcb_cb=136),
        #    size=1,
        #),
        # for next run, green will not be used
        # Red orange original, unmerged
        #RecoveryColor(
        #    name='orange',
        #    tube_color=ComplexColor(lab_a=148, lab_b=146, ycrcb_cr=154, ycrcb_cb=112),
        #    ellipse_color=ComplexColor(lab_a=131, lab_b=120, ycrcb_cr=127, ycrcb_cb=134),
        #    size=2/3,
        #),
    ]
)

torpedoes = Torpedoes(
    depth=2.5,
)

# TRANSDEC PREDICTION BASED ON FOOTAGE
buoysVision = BuoysVision(
    hls_l_min=0,
    hls_l_max=255,
    red_hls_h_min=0,
    red_hls_h_max=57,
    red_ycrcb_cb_min=0,
    red_ycrcb_cb_max=107,
    red_lab_a_min=118,
    red_lab_a_max=255,
    red_lab_b_min=145,
    red_lab_b_max=230,
    red_erode_kernel_size=1,
    red_dilate_kernel_size=1,
    green_lab_l_min=185, #169, 201,
    green_lab_l_max=255,
    green_hsv_v_min=212,
    green_hsv_v_max=255,
    green_hsv_h_min=0,
    green_hsv_h_max=255,
    green_ycrcb_y_min=178, #155, 168,
    green_ycrcb_y_max=255,
    green_erode_kernel_size=4,
    green_dilate_kernel_size=8,
    yellow_hls_h_min=0,
    yellow_hls_h_max=50,
    yellow_ycrcb_cb_min=0,
    yellow_ycrcb_cb_max=106,
    yellow_lab_a_min=114,
    yellow_lab_a_max=255,
    yellow_lab_b_min=151,
    yellow_lab_b_max=255,
    yellow_erode_kernel_size=3,
    yellow_dilate_kernel_size=9,
    blur_kernel=1,
    max_error=5000,
    max_buoy_error=1500,
    min_circularity=0.48,
    min_percent_frame=0.001,
    ex=243,
    ey=171,
    max_y_diff=150,
    min_x_diff=50,
)

gate = Gate(
    depth=1.5,
    distance=20,
    vel_time=36,
    artemis_vel_time=35,
)
