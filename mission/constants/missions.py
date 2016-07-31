from collections import namedtuple

Navigate = namedtuple('Navigate', [
    'max_distance',
    'debugging',
    'thresh_c',
    'kernel_size',
    'min_area',
    'block_size',
])

Recovery = namedtuple('Recovery', [
    'stack_dive_altitude',
    'vstack_grab_altitude',
    'hstack_grab_altitude',

    'debugging',
    'table_c',
    'table_block_size',
    'table_min_area',
    'table_min_fill_ratio',
    'red_stack_c',
    'green_stack_c',
    'red_mark_c',
    'green_mark_c',
    'min_stack_area',
    'min_stack_width',
])
