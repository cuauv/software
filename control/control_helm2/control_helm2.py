#!/usr/bin/env python3

import sys
import time

from helm_basis import *

import shm

def build_control_helm(expert=False):
    BATTERY_LOW = 14.2
    BATTERY_EMPTY = 13.0

    assert BATTERY_EMPTY <= BATTERY_LOW

    default_msg = 'EXPERT MODE' if expert else ''

    msg = default_msg
    buf = ''

    def dvl_fmt(val):
        return '{:6.2f}'.format(val)

    def pid_panel(name, rate_name, nav_desire_name):
        desire = getattr(shm.navigation_desires, nav_desire_name)
        value = getattr(shm.kalman, name)
        internal = getattr(shm, 'control_internal_{}'.format(name))
        locked = getattr(shm.control_locked, name)
        rate = getattr(shm.kalman, rate_name)
        settings = getattr(shm, 'settings_{}'.format(name))
        active = getattr(shm.settings_control, '{}_active'.format(name))

        def rd():
            v = auto_shm_val_fmt(settings.rD.get())
            # highlight RD if locked
            return StyledString('[{}]'.format(v)) if locked.get() else v

        return LineLambdaPanel([
            lambda: (' DES:', auto_shm_val_fmt(desire.get()),
                     '   P:', auto_shm_val_fmt(settings.kP.get())),
            lambda: (' VAL:', auto_shm_val_fmt(value.get()),
                     '   I:', auto_shm_val_fmt(settings.kI.get())),
            lambda: (' OUT:', auto_shm_val_fmt(internal.out.get()),
                     '   D:', auto_shm_val_fmt(settings.kD.get())),
            lambda: (' RTE:', auto_shm_val_fmt(rate.get()),
                     '  IG:', auto_shm_val_fmt(internal.integral.get())),
            lambda: (StyledString('   [ON]') if active.get() else '   ON',
                     '  OFF' if active.get() else StyledString('  [OFF]'),
                     '  RD:', rd()),
        ], title=name, width=26, columns=True, padding=False)

    def get_battery_status():
        nonlocal BATTERY_LOW, BATTERY_EMPTY

        status = shm.merge_status.get()
        if status.total_current == 0 and status.total_voltage == 0:
            return ('   Not on vehicle.', ' Monitoring disabled.')
        else:
            voltage_line = '        {:5.2f}V'.format(status.total_voltage)
            if status.total_voltage < BATTERY_EMPTY:
                # manual blinking; we can use ncurses blinking, but it doesn't
                # blink the background
                status_line = \
                    StyledString('$<white,{}>   REPLACE BATTERIES  $'
                                 .format('red' if (time.time() * 10) % 1 < 0.5 \
                                         else 'black'))
            elif status.total_voltage < BATTERY_LOW:
                status_line = \
                        StyledString('$<black,yellow>     Low voltages.    $')
            else:
                status_line = '  Voltages nominal.'
            return (voltage_line, status_line)

    drive_panels = Vbox(
        Hbox(
            LineLambdaPanel([
                lambda: ' PORT: {:4}  '.format(
                    shm.motor_desires.port.get()),
                lambda: ' STAR: {:4}  '.format(
                    shm.motor_desires.starboard.get()),
                lambda: ' FORE:{:3}:{:3} '.format(
                    shm.motor_desires.fore_port.get(),
                    shm.motor_desires.fore_starboard.get()),
                lambda: '  AFT:{:3}:{:3} '.format(
                    shm.motor_desires.aft_port.get(),
                    shm.motor_desires.aft_starboard.get()),
                lambda: ' SFOR: {:4}  '.format(
                    shm.motor_desires.sway_fore.get()),
                lambda: ' SAFT: {:4}  '.format(
                    shm.motor_desires.sway_aft.get()),
            ], width=16, padding=False),

            ShmPanel(shm.navigation_desires, width=20, title=None,
                     padding=False,
                     select_vars=['heading', 'depth', 'pitch',
                                  'roll', 'speed', 'sway_speed'],
                     var_names=[' DES HEAD', ' DES DPTH', ' DES PTCH',
                                ' DES ROLL', ' DES VELX', ' DES VELY']),

            ShmPanel(shm.kalman, width=16, title=None, padding=False,
                     select_vars=['heading', 'depth', 'pitch',
                                  'roll', 'velx', 'vely'],
                     var_names=[' HEAD', ' DPTH', ' PTCH',
                                ' ROLL', ' VELX', ' VELY']),

            LineLambdaPanel([
                lambda: (' DVL ALTD:', dvl_fmt(shm.dvl.savg_altitude.get())),
                lambda: (' DVL TEMP:', dvl_fmt(shm.dvl.temperature.get())),
                lambda: (StyledString.highlight_if(
                    ' DVL BEAM 1', shm.dvl.low_amp_1.get()
                    or shm.dvl.low_correlation_1.get()), '  FWRD:'),
                lambda: (StyledString.highlight_if(
                    ' DVL BEAM 2', shm.dvl.low_amp_2.get()
                    or shm.dvl.low_correlation_2.get()),
                         dvl_fmt(shm.kalman.forward.get())),
                lambda: (StyledString.highlight_if(
                    ' DVL BEAM 3', shm.dvl.low_amp_3.get()
                    or shm.dvl.low_correlation_3.get()), '  SWAY:'),
                lambda: (StyledString.highlight_if(
                    ' DVL BEAM 4', shm.dvl.low_amp_4.get()
                    or shm.dvl.low_correlation_4.get()),
                         dvl_fmt(shm.kalman.sway.get())),
            ], width=20, columns=True, padding=False),

            LineLambdaPanel([
                lambda: StyledString.highlight_if(
                    ' HK ',shm.switches.hard_kill.get()),
                lambda: StyledString.highlight_if(
                    ' SK ', shm.switches.soft_kill.get()),
                lambda: StyledString.highlight_if(
                    ' DV ', shm.dvl.vel_x_invalid.get()
                    or shm.dvl.vel_y_invalid.get()
                    or shm.dvl.vel_z_invalid.get()),
                lambda: StyledString.highlight_if(
                    ' PC ', shm.navigation_settings.position_controls.get()),
                lambda: StyledString.highlight_if(
                    ' OT ', shm.navigation_settings.optimize.get()),
                lambda: StyledString.highlight_if(
                    ' EN ', shm.settings_control.enabled.get()),
            ], width=6, padding=False),
            height=8, min_height=8
        ),

        # PID loop panels
        Hbox(
            pid_panel('heading', 'heading_rate', 'heading'),
            pid_panel('pitch', 'pitch_rate', 'pitch'),
            pid_panel('roll', 'roll_rate', 'roll'),
            height=8,
        ),
        Hbox(
            pid_panel('velx', 'accelx', 'speed'),
            pid_panel('vely', 'accely', 'sway_speed'),
            pid_panel('depth', 'depth_rate', 'depth'),
            height=8,
        ),

        Hbox(
            LineLambdaPanel([
                lambda: get_battery_status()[0],
                lambda: get_battery_status()[1],
            ], width=26),
            LineLambdaPanel([
                lambda: msg,
                lambda: buf,
            ], width=26),
            Panel(width=26),
            height=4,
        ),
    )

    def toggle_shm(var, set_msg=None):
        nonlocal msg
        var.set(not var.get())
        if set_msg is not None:
            msg = set_msg

    def zero(surface):
        nonlocal msg
        desires = shm.navigation_desires.group()
        kalman = shm.kalman.get()
        desires.heading = kalman.heading
        desires.north = kalman.north
        desires.east = kalman.east
        if not surface:
            desires.depth = kalman.depth
        shm.navigation_desires.set(desires)
        msg = 'Surface!' if surface else 'Zero movements'

    def soft_kill(killed):
        nonlocal msg
        shm.switches.soft_kill.set(killed)
        msg = 'KILLED' if killed else 'UNKILLED'

    def toggle_quaternion():
        nonlocal msg
        msg = 'Quaternions broken :('

    drive_callbacks = {
        ' ': (lambda: soft_kill(True)),
        curses.KEY_F5: (lambda: soft_kill(False)),
        '\\': (lambda: soft_kill(False)),
        curses.KEY_F12: (lambda: toggle_shm(shm.settings_control.enabled, 'Toggle Controller')),
        '|': (lambda: toggle_shm(shm.settings_control.enabled, 'Toggle Controller')),
        'n': (lambda: toggle_shm(shm.navigation_settings.position_controls, 'Toggle PosCon')),
        't': (lambda: toggle_shm(shm.navigation_settings.optimize, 'Toggle Trajectories')),
        'z': (lambda: zero(False)),
        'Z': (lambda: zero(True)),
        curses.KEY_LEFT: (lambda: shm.navigation_desires.heading.set(
            (shm.desires.heading.get() - 5) % 360)),
        curses.KEY_RIGHT: (lambda: shm.navigation_desires.heading.set(
            (shm.desires.heading.get() + 5) % 360)),
        curses.KEY_UP: (lambda: shm.navigation_desires.depth.set(
            shm.desires.depth.get() - 0.1)),
        curses.KEY_DOWN: (lambda: shm.navigation_desires.depth.set(
            shm.desires.depth.get() + 0.1)),
        curses.KEY_SLEFT: (lambda: shm.navigation_desires.sway_speed.set(
            shm.desires.sway_speed.get() - 0.1)),
        curses.KEY_SRIGHT: (lambda: shm.navigation_desires.sway_speed.set(
            shm.desires.sway_speed.get() + 0.1)),
        'u': (lambda: shm.navigation_desires.heading.set(
            (shm.desires.heading.get() + 180) % 360)),
        'a': (lambda: 'all'),
        'q': toggle_quaternion,
    }

    default_mode_callbacks = {}

    # for setting speeds
    numbers       = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
    numbers_shift = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')']

    def add_speed_callback(n, ns, speed):
        # speeds go into 'default' mode because we use the same keys for
        # setting values in the PID modes
        default_mode_callbacks[n] = \
            lambda: shm.navigation_desires.speed.set(speed)
        default_mode_callbacks[ns] = \
            lambda: shm.navigation_desires.speed.set(-speed)

    for i, (n, ns) in enumerate(zip(numbers, numbers_shift)):
        add_speed_callback(n, ns, 0.1 * i)

    def toggle_all():
        s = shm.settings_control.get()
        val = not s.heading_active
        s.heading_active = val
        s.pitch_active = val
        s.roll_active = val
        s.velx_active = val
        s.vely_active = val
        s.depth_active = val
        shm.settings_control.set(s)
        return 'default'

    drive_modal_callbacks = {
        'default': default_mode_callbacks,
        'all': {
            'o': toggle_all,
            chr(27): lambda: 'default',
            'c': lambda: 'default',
        },
    }

    def add_buffer_edit_callbacks(callback_dict, commit_callback,
                                  allowed_chars, quit_to_mode='default',
                                  quit_to_msg=default_msg):
        def quit_mode():
            nonlocal msg, buf
            msg = quit_to_msg
            buf = ''
            return quit_to_mode

        def backspace():
            nonlocal buf
            buf = buf[:-1]

        def add_char_callback(char):
            def cb():
                nonlocal buf
                buf += char
            return cb

        def do_commit():
            nonlocal msg, buf
            if commit_callback():
                return quit_mode()
            else:
                buf = ''
                msg = 'Invalid; try again'

        callback_dict.update({
            chr(27): quit_mode,
            'c': quit_mode,
            curses.KEY_BACKSPACE: backspace,
            '\b': backspace,
            '\n': do_commit,
            '\r': do_commit,
        })

        for c in allowed_chars:
            callback_dict[str(c)] = add_char_callback(str(c))

    def add_pid_callbacks(key, name, nav_desire_name, cvt):
        nonlocal msg, buf, expert

        def switch_mode():
            nonlocal msg, buf
            msg = 'Enter {}:'.format(name.title())
            buf = ''
            return name

        def commit():
            val = cvt(buf)
            if val is not None:
                getattr(shm.navigation_desires, nav_desire_name).set(val)
                return True
            return False

        def toggle_pid_loop():
            nonlocal msg, buf
            toggle_shm(getattr(shm.settings_control, '{}_active'.format(name)))
            msg = default_msg
            buf = ''
            return 'default'

        drive_modal_callbacks['default'][key] = switch_mode
        drive_modal_callbacks[name] = {
            'o': toggle_pid_loop
        }
        add_buffer_edit_callbacks(drive_modal_callbacks[name], commit,
                                  list(range(10)) + ['.', '-'])

        if expert:
            def add_expert_control(pid, constant_name):
                nonlocal drive_modal_callbacks

                mode_name = '{}_{}'.format(name, pid)

                def switch_constant_mode():
                    nonlocal msg, buf
                    msg = 'Enter {} {}:'.format(name.title(), constant_name)
                    buf = ''
                    return mode_name

                drive_modal_callbacks[name][pid] = switch_constant_mode

                def commit_const():
                    try:
                        val = float(buf)
                        getattr(getattr(shm, 'settings_{}'.format(name)),
                                constant_name).set(val)
                        return True
                    except ValueError:
                        pass
                    return False

                drive_modal_callbacks[mode_name] = {}
                add_buffer_edit_callbacks(drive_modal_callbacks[mode_name],
                                          commit_const, list(range(10)) + ['.'])

            add_expert_control('p', 'kP')
            add_expert_control('i', 'kI')
            add_expert_control('d', 'kD')

    def converter(min_val, max_val, mult=1):
        def f(val):
            try:
                val_f = float(val)
                if min_val <= val_f <= max_val:
                    return val_f * mult
                else:
                    return None
            except ValueError:
                return None

        return f

    add_pid_callbacks('h', 'heading', 'heading', cvt=converter(0, 360))
    add_pid_callbacks('p', 'pitch', 'pitch', cvt=converter(-90, 90))
    add_pid_callbacks('r', 'roll', 'roll', cvt=converter(-180, 180))
    add_pid_callbacks('x', 'velx', 'speed', cvt=converter(-10, 10, 0.1))
    add_pid_callbacks('y', 'vely', 'sway_speed', cvt=converter(-10, 10, 0.1))
    add_pid_callbacks('d', 'depth', 'depth', cvt=converter(-0.2, 3))

    def add_positional_controls():
        def switch_to_posn():
            nonlocal msg, buf
            msg = 'Enter north position:'
            buf = ''
            return 'posn'

        drive_callbacks['l'] = switch_to_posn

        def commit_pos(name):
            try:
                val = float(buf)
                getattr(shm.navigation_desires, name).set(val)
                return True
            except ValueError:
                pass
            return False

        drive_modal_callbacks['posn'] = {}
        drive_modal_callbacks['pose'] = {}
        allowable_chars = list(range(10)) + ['.', '-']
        add_buffer_edit_callbacks(drive_modal_callbacks['posn'],
                                  lambda: commit_pos('north'), allowable_chars,
                                  quit_to_mode='pose',
                                  quit_to_msg='Enter east position:')
        add_buffer_edit_callbacks(drive_modal_callbacks['pose'],
                                  lambda: commit_pos('north'), allowable_chars)

    add_positional_controls()

    return drive_panels, drive_callbacks, drive_modal_callbacks

if __name__ == '__main__':
    start_helm(*build_control_helm(expert='-e' in sys.argv))
