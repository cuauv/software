from mission.constants.missions import Gate, Path, Dice, Highway, Track, Roulette, CashIn

from mission.missions.will_common import is_mainsub

HYDROPHONES_PINGER_DEPTH = 4.7

NONSURFACE_MIN_DEPTH = 1.0 if is_mainsub() else 1.5

# Note: These values are copied straight from the Teagle configuration.
# They need to be updated for Transdec!!

gate = Gate(
    depth=2.0,
    gate_width_threshold=0.4,
    charge_dist=16 if is_mainsub() else 12
)

path = Path(
    depth=1.2,
    search_forward=6 if is_mainsub() else 6,
    search_stride = 10 if is_mainsub() else 8,
    search_right_first=True,
    search_speed=0.1,
    post_dist=2.5,
    failure_back_up_dist=0.5 if is_mainsub() else 0.1,
    failure_back_up_speed=0.2 if is_mainsub() else 0.1,
)

dice = Dice(
    depth=3.3,
    max_depth=4,
    search_forward=4,
    search_stride=8,
    search_speed=0.2,
    min_dot_radius=0.04,
    ram_dist=1.5,
    ram_speed=0.16,
    rammed_back_up_timeout=20,
    lost_sight_back_up_timeout=5,
    search_default_zero_timeout=60,
)

highway = Highway(
    high_depth=1.6,
    low_depth=1.6,
    dist=6 if is_mainsub() else 2,
    speed=0.4 if is_mainsub() else 0.2,
)

track = Track(
    depth=2.2 if is_mainsub() else 2.2,
    slow_down_dist=5,
    max_speed=0.3 if is_mainsub() else 0.2,
    min_speed=0.1,
    vision_frame_period=0.5,
)

roulette = Roulette(
    depth_search=1.5,
    depth_realign=3.2,
    depth_drop=3.9,
    heading_offset=-30,
)

cash_in = CashIn(
    approach_funnel_depth=0.5,
    drop_approach_dist=0.2,
    # (right, left)
    drop_dvl_forward_correct_dist=(0.1, -0.13),
    drop_heading_correct=(0, -7),
    pick_up_both_depth=2.0,
    pick_up_search_depth_1=2.5,
    pick_up_search_depth_2=3.0,
    pick_up_search_depth_3=3.5,
    pick_up_start_follow_depth=4.5,
    attempt_surface_depth=-1,
    attempt_funnel_depth=0,
)
