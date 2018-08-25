from runtime import *
from test import vehicle, level, environment, Test, CASTOR, POLLUX, ERR, WARN, WATER, LAND
from gevent import sleep
from mission.constants.region import  PINGER_FREQUENCY
import shutil, os
import conf

# Sensors
@vehicle(POLLUX)
class GX4(Test):
    def online():
        return is_changing(shm.gx4.packets_received.get)

    def updating():
        return is_changing(shm.gx4.roll.get)

@vehicle(CASTOR)
class GX5(Test):
    def online():
        return is_changing(shm.gx4.packets_received.get)

    def updating():
        return is_changing(shm.gx4.roll.get)

@vehicle(CASTOR)
class DVL(Test):
    def ticking():
        return is_changing(shm.dvl.tick.get)

class Vision(Test):
    def forward_capture_source_present():
        return shell('test -f /dev/shm/auv_visiond-forward').code == 0

    def downward_capture_source_present():
        return shell('test -f /dev/shm/auv_visiond-downward').code == 0

    def poster_getting_forward():
        return is_changing(shm.poster_status.forward_counter.get)

    def poster_getting_downward():
        return is_changing(shm.poster_status.downward_counter.get)

    def modules_processing():
        changing_forward = is_changing(shm.poster_status.forward_counter.get)
        changing_downward = is_changing(shm.poster_status.downward_counter.get)
        return changing_forward and changing_downward

class Depth(Test):
    def updating():
        return is_changing(shm.depth.depth.get)

    def not_crazy():
        return abs(shm.depth.depth.get() - delayed(0.2, shm.depth.depth.get)) < 0.2

class Pressure(Test):
    def valid():
        return .7 < shm.pressure.hull.get() < .89

    def updating():
        return is_changing(shm.pressure.hull.get)

# Serial
class Serial(Test):
    def gpio_connected():
        return shm.connected_devices.Sensor.get()

    def merge_connected():
        return shm.connected_devices.merge.get()

    def thrusters_connected():
        return shm.connected_devices.thrusters.get()

    @vehicle(CASTOR)
    def thrusters2_connected():
        return shm.connected_devices.thrusters2.get()

    def power_distribution_connected():
        return shm.connected_devices.PD.get()

class System(Test):
    def cpu_usage_resonable():
        return float(shell('mpstat | tail -n 1 | sed "s/\s\s*/ /g" | cut -d" " -f4').stdout) < 600.0

    def disk_space_available():
        return shutil.disk_usage(os.environ["CUAUV_LOG"]).free > 10 * 1000 ** 3 # 10GB

    def services_up():
        # Please forgive me for this.
        service_down_color = '[1;31m'
        return service_down_color not in str(shell('trogdor').stdout)

class Hydrophones(Test):
    def board_talking():
        return is_changing(shm.hydrophones_status.packet_count.get)

    @environment(WATER)
    def getting_pings():
        freq = shm.hydrophones_settings.track_frequency_target.get()
        shm.hydrophones_settings.track_frequency_target.set(PINGER_FREQUENCY)
        ret = is_changing(shm.hydrophones_results_track.tracked_ping_count.get, 500)
        shm.hydrophones_settings.track_frequency_target.set(freq)
        return ret

@environment(WATER)
class Trim(Test):
    @level(WARN)
    def pitch_trim():
        return abs(shm.kalman.pitch.get()) < 3.0

    @level(WARN)
    def roll_trim():
        return abs(shm.kalman.roll.get()) < 3.0

# Daemons
class Kalman(Test):
    def heading_updating():
        return is_changing(shm.kalman.heading.get)

    def heading_valid():
        return 0 <= shm.kalman.heading.get() < 360

class Controller(Test):
    @level(WARN)
    def all_thrusters_enabled():
        from control.thrusters import all_thrusters
        return not any([t.broken for t in all_thrusters])

# Other
class Merge(Test):
    def total_voltage_ok():
        return 17 > shm.merge_status.total_voltage.get() > 14.0

import syscheck_selftest
