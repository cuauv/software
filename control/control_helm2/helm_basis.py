#!/usr/bin/env python3

import curses
import time
import math
from collections import namedtuple

import shm

# TODO:
#  - efficiency improvements
#  - make code prettier
#  - make a better control helm layout, using the fancy new features?

class StyledString(str):
    """
    Just like a regular string, but with the following style delimiters:
    '*bold text*, @underlined text@, [highlighted text], !blinking text!'.
    They are nestable. Also can do colors: '$<white,red>text$' for white
    text and red background.
    """

    def highlight_if(text, b):
        return StyledString('[{}]'.format(text) if b else text)

# Represents the position of a panel on the screen
Position = namedtuple('Position', ['x', 'y', 'width', 'height'])

def make_position(hv_ind, val_pos, non_val_pos, val_size, non_val_size):
    """
    Handles selecting position of coordinates (horizontal or vertical) based
    on :hv_ind:, i.e. horizontal/vertical indicator. (0 for horizontal, 1 for
    vertical.) I.e. make_position(0, a, b, c, d) = Position(a, b, c, d) and
    make_position(1, a, b, c, d) = Position(b, a, d, c).

    :val_pos:/:non_val_pos: are x/y.
    :val_size:/:non_val_size: are width/height.
    """
    if hv_ind:
        return Position(non_val_pos, val_pos, non_val_size, val_size)
    else:
        return Position(val_pos, non_val_pos, val_size, non_val_size)

class Panel():
    """
    A panel is the basic unit of display on the screen. It is surrounded y a
    box.
    """

    def __init__(self, title=None, width=None, height=None, min_width=0,
                 max_width=math.inf, min_height=0, max_height=math.inf,
                 weight=1, padding=True):

        assert isinstance(weight, int)

        self.title = title
        self.min_width = min_width
        self.min_height = min_height
        self.max_width = width if width is not None else max_width
        self.max_height = height if height is not None else max_height
        self.weight = weight
        self.padding = padding

    def min_dim(self):
        return (self.min_width, self.min_height)

    def max_dim(self):
        return (self.max_width, self.max_height)

    def max_dim_bounded(self):
        return (self.max_width < math.inf, self.max_height < math.inf)

    def get_cols_lines(self, width, height):
        """
        Returns a list of columns, each of which is in turn a list of lines to
        display. :width: and :height: are the maximum size available
        """
        return []

class LineLambdaPanel(Panel):
    """
    A panel that uses a list of content producers to produce each line. If
    :columns:, then the result is pivoted; i.e. a list of rows, each containing
    multiple cells, is converted into a list of columns, each containing
    multiple cells (as required by get_cols_lines() spec).
    """

    def __init__(self, line_lambdas, title=None, columns=False, *args, **kwargs):
        assert isinstance(line_lambdas, list)
        super().__init__(title, *args, **kwargs)
        self.line_lambdas = line_lambdas
        self.columns = columns

    def get_cols_lines(self, width, height):
        out = [f() for f in self.line_lambdas]
        # if self.columns, then pivot output
        # TODO wrap output into multiple columns if it exceeds box height
        return list(map(list, zip(*out))) if self.columns else [out]

def auto_shm_val_fmt(val):
    """
    Returns :val: formatted so that numbers are aligned.
    """
    if isinstance(val, int):
        return '{:3}'.format(val)
    elif isinstance(val, float):
        return '{:7.2f}'.format(val)
    elif isinstance(val, StyledString):
        return val
    else:
        return str(val)

class ShmPanel(LineLambdaPanel):
    """
    A panel that displays a SHM group. Must specify either :group: or
    :variables:, but not both.

    :group: - SHM group to display
    :variables: - list of SHM variables to display
    :select_vars: - optional variable filter (list of variable names),
                    only used when using :group:
    :var_names: - optional variable renaming (list of new names)
    :val_fmt: - function to use for formatting returned values as strings
    """

    def __init__(self, group=None, variables=None, select_vars=None, var_names=None,
                 title='', val_fmt=auto_shm_val_fmt, *args, **kwargs):
        assert (group is not None) ^ (variables is not None)
        if variables is not None:
            assert isinstance(variables, list)
            assert title is not None

        # we can get the title from the group if it is present
        # but let the user disable title by setting to None
        if group is not None and title == '':
            title = group.__name__
        if title == '':
            title = None

        if group is not None:
            if select_vars is None:
                # select all variables
                variables = [getattr(group, field[0]) for field in group._fields]
            else:
                # select only variables specified by select_vars
                variables = [getattr(group, var) for var in select_vars]

        if var_names is not None:
            # apply renamings
            var_name_map = {var: name for var, name in zip(variables, var_names)}
        else:
            var_name_map = {}

        def make_row(v):
            return lambda: ('{}:'.format(var_name_map.get(v, v.__name__)), val_fmt(v.get()))

        line_lambdas = list(map(make_row, variables))

        super().__init__(title=title, line_lambdas=line_lambdas, columns=True, *args, **kwargs)

class Layout(Panel):
    """
    A layout contains panels or other layouts. It defines a list of contents and
    a procedure for determining positions and dimensions for all of its contents
    given a set of dimensions.
    """

    def __init__(self, *contents, **kwargs):
        super().__init__('layout', **kwargs)
        self._contents = contents

    def contents(self):
        """
        Returns the list of contents (layouts or panels).
        """
        return self._contents

    def layout(self, width, height):
        """
        Returns a dict mapping contents to positions.
        """
        return {panel: Position(0, 0, width, height) for panel in self.contents()}

class LinearBox(Layout):
    """
    Lays out contents either horizontally or vertically.
    """

    def __init__(self, vert, *contents, **kwargs):
        super().__init__(*contents, **kwargs)
        # horizontal/vertical indicator (True if vertical)
        self.hv_ind = bool(vert)

        self.cached_dim = None
        self.cached_result = None

        # (width, height), (min, max) - used for calculating appropriate total
        # min/max dimensions
        whmm_funs = ((sum, sum), (lambda x: min(x, default=math.inf),
                                  lambda x: max(x, default=0)))

        self.min_width = max(self.min_width, whmm_funs[not self.hv_ind][0](
            map(lambda p: p.min_width, self.contents())))
        self.max_width = min(self.max_width, whmm_funs[not self.hv_ind][1](
            map(lambda p: p.max_width, self.contents())))
        self.min_height = max(self.min_height, whmm_funs[self.hv_ind][0](
            map(lambda p: p.min_height, self.contents())))
        self.max_height = min(self.max_height, whmm_funs[self.hv_ind][1](
            map(lambda p: p.max_height, self.contents())))

    def layout(self, width, height):
        total_dim = (width, height)

        # don't re-calculate positions if dimensions haven't changed
        if total_dim != self.cached_dim:
            total_bounded_max = 0
            total_bounded_weight = 0
            total_unbounded_weight = 0
            for panel in self.contents():
                if panel.max_dim_bounded()[self.hv_ind]:
                    total_bounded_max += panel.max_dim()[self.hv_ind]
                    total_bounded_weight += panel.weight
                else:
                    total_unbounded_weight += panel.weight
            free_val = total_dim[self.hv_ind] - total_bounded_max

            free_val_per_panel = free_val // total_unbounded_weight \
                if total_unbounded_weight != 0 else 0
            roundoff_leftover = \
                free_val - free_val_per_panel * total_unbounded_weight

            over_full = free_val < 0
            over_full_sub_per_panel = free_val // total_bounded_weight \
                if total_bounded_weight != 0 else 0
            over_full_roundoff_leftover = \
                free_val - over_full_sub_per_panel * total_bounded_weight

            val_counter = 0
            result = {}
            for panel in self.contents():
                if over_full:
                    if panel.max_dim_bounded()[self.hv_ind]:
                        panel_val = int(panel.max_dim()[self.hv_ind]) \
                            + over_full_sub_per_panel * panel.weight
                        if over_full_roundoff_leftover > 0:
                            panel_val += 1
                            over_full_roundoff_leftover -= 1
                    else:
                        panel_val = 0
                else:
                    if panel.max_dim_bounded()[self.hv_ind]:
                        panel_val = int(panel.max_dim()[self.hv_ind])
                    else:
                        panel_val = free_val_per_panel * panel.weight
                        if roundoff_leftover > 0:
                            panel_val += 1
                            roundoff_leftover -= 1
                panel_val = min(max(panel_val, panel.min_dim()[self.hv_ind]),
                                total_dim[self.hv_ind] - val_counter)
                result[panel] = make_position(self.hv_ind, val_counter, 0,
                                              panel_val,
                                              total_dim[not self.hv_ind])
                val_counter += panel_val

            self.cached_result = result

        return self.cached_result

class Hbox(LinearBox):
    """
    Lays out contents horizontally. See LinearBox.
    """
    def __init__(self, *contents, **kwargs):
        super().__init__(False, *contents, **kwargs)

class Vbox(LinearBox):
    """
    Lays out contents vertically. See LinearBox.
    """
    def __init__(self, *contents, **kwargs):
        super().__init__(True, *contents, **kwargs)

# Map of color names to ncurses color constants
color_map = {
    'black': curses.COLOR_BLACK,
    'red': curses.COLOR_RED,
    'green': curses.COLOR_GREEN,
    'yellow': curses.COLOR_YELLOW,
    'blue': curses.COLOR_BLUE,
    'magenta': curses.COLOR_MAGENTA,
    'cyan': curses.COLOR_CYAN,
    'white': curses.COLOR_WHITE,
}

def get_color(c):
    """
    Get an ncurses color constant by name or number.
    """
    if c in color_map:
        return color_map[c]
    else:
        try:
            return int(c)
        except ValueError:
            raise Exception('Invalid color: {}'.format(c))

def text_draw(box, text, max_chars):
    """
    Draw :text: in the ncurses window :box:, up to a maximum of :max_chars:.
    Handles formatting of StyledStrings.
    """

    if isinstance(text, StyledString):
        pos = 0 # pos into string
        pos_disp = 0 # number of chars displayed so far
        attr = 0
        color = 0

        while pos < len(text) and pos_disp < max_chars:
            c = text[pos]
            if c == '*':
                attr ^= curses.A_BOLD
            elif c == '[' and not attr & curses.A_STANDOUT:
                attr ^= curses.A_STANDOUT
            elif c == ']' and attr & curses.A_STANDOUT:
                attr ^= curses.A_STANDOUT
            elif c == '@':
                attr ^= curses.A_UNDERLINE
            elif c == '!':
                attr ^= curses.A_BLINK
            elif c == '$':
                if color:
                    # reset color
                    color = 0
                else:
                    # parse text color format of the form:
                    # '$<fg,bg>text$'
                    pos += 1
                    assert text[pos] == '<'

                    fg_end = text.index(',', pos)
                    fg = get_color(text[pos + 1:fg_end])
                    pos = fg_end

                    bg_end = text.index('>', pos)
                    bg = get_color(text[pos + 1:bg_end])
                    pos = bg_end

                    pair = curses.init_pair(1, fg, bg)
                    color = curses.color_pair(1)
            else:
                box.addstr(c, attr | color)
                pos_disp += 1
            pos += 1
        return pos_disp
    else:
        assert text is not None
        text = text[:max_chars]
        box.addstr(text)
        return len(text)

def panel_draw(screen, panel, pos):
    """
    Draw a panel or layout.
    """

    if isinstance(panel, Layout):
        # this is a layout, so recursively draw children

        layout = panel.layout(pos.width, pos.height)
        for child, child_pos in layout.items():
            panel_draw(screen, child,
                       Position(pos.x + child_pos.x, pos.y + child_pos.y,
                                child_pos.width, child_pos.height))
    elif isinstance(panel, Panel):
        # this is a panel, so draw the panel

        try:
            box = screen.subwin(pos.height, pos.width, pos.y, pos.x)
        except curses.error:
            # it glitches right after resizing
            return

        # draw box around panel
        box.box()

        # one space to either side, one slot taken by box
        text_width = pos.width - 4 if panel.padding else pos.width - 2
        # one slot take by box on top and bottom, one slot take by title
        text_height = pos.height - 2
        text_start_x = 2 if panel.padding else 1
        text_start_y = 1

        if panel.title is not None:
            # display title, centered on first line
            panel_title = panel.title[:text_width]
            panel_title_xpos = \
                2 + math.floor(text_width / 2 - len(panel_title) / 2)
            try:
                box.move(1, panel_title_xpos)
                text_draw(box,
                          StyledString('*{}*'.format(panel_title)), text_width)
            except curses.error:
                pass
            text_height -= 1
            text_start_y += 1

        # this gives us a list of columns, each of which is a list of rows
        # (each containing one cell)
        cols = panel.get_cols_lines(text_width, text_height)
        x_offset = 0
        for col_num, lines in enumerate(cols):
            col_width = 0
            for line_num, line in enumerate(lines[:text_height]):
                try:
                    box.move(line_num + text_start_y, x_offset + text_start_x)
                    line_width = text_draw(box, line,
                                           max(text_width - x_offset, 0))
                    col_width = max(col_width, line_width)
                except curses.error:
                    pass
            # keep one space between columns (if padding enabled)
            x_offset += col_width + (1 if panel.padding else 0)
    else:
        raise Exception('Not a panel or layout!')

def main(screen, panels, callbacks, modal_callbacks, loop_delay):
    """
    Main loop. See start_helm for more information.
    """

    # mode is used for switching between different input modes
    # this is controlled entirely by the provided callbacks
    assert 'default' in modal_callbacks
    mode = 'default'

    curses.curs_set(False)
    screen.nodelay(True)

    while True:
        # detect resizes
        screen_height, screen_width = screen.getmaxyx()

        # process all buffered key presses
        key = screen.getch()
        while key != curses.ERR:
            callback = None

            if key == curses.KEY_RESIZE:
                curses.update_lines_cols()
            elif key in callbacks:
                callback = callbacks[key]
            elif chr(key) in callbacks:
                callback = callbacks[chr(key)]
            elif key in modal_callbacks[mode]:
                callback = modal_callbacks[mode][key]
            elif chr(key) in modal_callbacks[mode]:
                callback = modal_callbacks[mode][chr(key)]

            if callback is not None:
                # invoke callback
                # may optionally return a value to change the mode
                ret = callback()
                if ret is not None:
                    if not ret in modal_callbacks:
                        raise Exception('{} is not a valid mode'.format(ret))
                    mode = ret

            key = screen.getch()

        screen.erase()

        panel_draw(screen, panels, Position(0, 0, screen_width, screen_height))

        screen.refresh()
        time.sleep(loop_delay)

def start_helm(panels, callbacks={}, modal_callbacks={'default': {}},
               loop_delay=0.075):
    """
    Start a helm, as defined by:
    :panels: - the top-level panel or layout
    :callbacks: - a map of characters (or curses key contants) to callback
                  functions; a function may optionally return a value to switch
                  to the mode corresponding to that value
    :modal_callbacks: - a map of modes (e.g. strings) to callback maps for that
                        mode; must contain the 'default' mode, which is loaded
                        at start
    :loop_delay: - time to wait between each display cycle (in seconds)
    """

    try:
        curses.wrapper(main, panels, callbacks, modal_callbacks, loop_delay)
    except KeyboardInterrupt:
        pass
