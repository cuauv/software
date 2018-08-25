from collections import namedtuple

Gate = namedtuple('Gate', [
    'depth',
    'gate_width_threshold',
    'charge_dist',
])

Path = namedtuple('Path', [
    'depth',
    'search_forward',
    'search_stride',
    'search_right_first',
    'search_speed',
    'post_dist',
    'failure_back_up_dist',
    'failure_back_up_speed',
])

Dice = namedtuple('Dice', [
    'depth',
    'max_depth',
    'search_forward',
    'search_stride',
    'search_speed',
    'min_dot_radius',
    'ram_dist',
    'ram_speed',
    'rammed_back_up_timeout',
    'lost_sight_back_up_timeout',
    'search_default_zero_timeout',
])

Highway = namedtuple('Highway', [
    'high_depth',
    'low_depth',
    'dist',
    'speed',
])

Track = namedtuple('Track', [
    'depth',
    'slow_down_dist',
    'max_speed',
    'min_speed',
    'vision_frame_period',
])

Roulette = namedtuple('Roulette', [
    'depth_search',
    'depth_realign',
    'depth_drop',
    'heading_offset',
])

CashIn = namedtuple('CashIn', [
    'approach_funnel_depth',
    'drop_approach_dist',
    'drop_dvl_forward_correct_dist',
    'drop_heading_correct',
    'pick_up_both_depth',
    'pick_up_search_depth_1',
    'pick_up_search_depth_2',
    'pick_up_search_depth_3',
    'pick_up_start_follow_depth',
    'attempt_surface_depth',
    'attempt_funnel_depth',
])
