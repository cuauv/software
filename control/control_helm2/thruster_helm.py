#!/usr/bin/env python3

from helm_basis import *

import shm

# auv-thruster-helm: Easy controls for changing reversed/broken thruster status in SHM.
#
# TODO: perhaps integrate some other things:
#  - start thruster test directly from thruster helm?
#  - display which thruster board and ECE-side variable each thruster is connected to?
#      (see thruster mapper for how to do this)

BOX_WIDTH = 24
BOX_HEIGHT = 8


def build_thruster_helm():
    thrusters = [name for name, typ in shm.motor_desires._fields]

    def highlight_shm_if(text, shm_group, shm_var):
        return StyledString.highlight_if(text, getattr(shm_group, shm_var).get())

    index = 0
    thruster_index_map = {}

    def get_index(name):
        nonlocal index
        index += 1
        thruster_index_map[name] = index
        return str(index) + ': ' + name

    def make_thruster_panel(thruster):
        return LineLambdaPanel([
            lambda: highlight_shm_if(
                'reversed', shm.reversed_thrusters, thruster),
            lambda: highlight_shm_if(
                'broken', shm.broken_thrusters, thruster),
            lambda: highlight_shm_if(
                'spinning', shm.motor_desires, thruster),
            lambda: (
                'PWM: ' +
                str(getattr(shm.motor_desires, thruster).get())
            ),
        ], title=get_index(thruster), width=BOX_WIDTH)

    panels = Vbox(
        Hbox(
            LineLambdaPanel([
                lambda: (
                    StyledString.highlight_if(
                        'SK', shm.switches.soft_kill.get())
                    + ' ' +
                    StyledString.highlight_if(
                        'HK', shm.switches.hard_kill.get())
                    + ' ' +
                    StyledString.highlight_if(
                        'EN', shm.settings_control.enabled.get())
                    + ' -- mode: ' +
                    StyledString.highlight_if(
                        'reversed', mode == 'reversed')
                    + ' {r} / ' +
                    StyledString.highlight_if(
                        'broken', mode == 'broken')
                    + ' {b} / ' +
                    StyledString.highlight_if(
                        'spinning', mode == 'spinning')
                    + ' {s}'
                ),
            ], title=None, width=BOX_WIDTH * 4, height=3),
        ),
        Hbox(*map(make_thruster_panel,
             thrusters[:len(thrusters)//2]),
             height=BOX_HEIGHT),
        Hbox(*map(make_thruster_panel,
             thrusters[len(thrusters)//2:]),
             height=BOX_HEIGHT),
        Hbox(LineLambdaPanel([
            lambda: StyledString(
                'Press {{n}} key to toggle [{}] status for thruster {{n}}'
                .format(mode))],
            title=None, width=BOX_WIDTH * 4, height=3))
    )

    def soft_kill(killed):
        shm.switches.soft_kill.set(killed)

    def toggle_controller():
        shm.settings_control.enabled.set(
            not shm.settings_control.enabled.get())

    def toggle_reversed(thruster):
        var = getattr(shm.reversed_thrusters, thruster)
        var.set(not var.get())

    def toggle_broken(thruster):
        var = getattr(shm.broken_thrusters, thruster)
        var.set(not var.get())

    def toggle_spinning(thruster):
        var = getattr(shm.motor_desires, thruster)
        var.set(0 if var.get() else 30)

    callbacks = {
        ' ': (lambda: soft_kill(True)),
        curses.KEY_F5: (lambda: soft_kill(False)),
        '|': toggle_controller,
        curses.KEY_F12: toggle_controller,
    }

    modal_callbacks = {}

    # janky way to keep track of state
    # TODO: provide way to access current mode from helm_basis.py
    mode = 'reversed'

    def change_mode(new_mode):
        nonlocal mode
        mode = new_mode
        return mode

    # reversed
    modal_callbacks['reversed'] = {
        'b': lambda: change_mode('broken'),
        's': lambda: change_mode('spinning'),
    }
    # another hacky thing
    modal_callbacks['default'] = modal_callbacks['reversed']

    # broken
    modal_callbacks['broken'] = {
        'r': lambda: change_mode('reversed'),
        's': lambda: change_mode('spinning'),
    }

    # spinning
    modal_callbacks['spinning'] = {
        'b': lambda: change_mode('broken'),
        'r': lambda: change_mode('reversed'),
    }

    def add_thruster_callbacks(thruster, index):
        modal_callbacks['default'][str(index)] = \
            lambda: toggle_reversed(thruster)
        modal_callbacks['broken'][str(index)] = \
            lambda: toggle_broken(thruster)
        modal_callbacks['spinning'][str(index)] = \
            lambda: toggle_spinning(thruster)

    for thruster, index in thruster_index_map.items():
        add_thruster_callbacks(thruster, index)

    return panels, callbacks, modal_callbacks


if __name__ == '__main__':
    start_helm(*build_thruster_helm())
