# Tests that didn't neatly fit in another file. Should be deprecated eventually.

# #@ sensors.him.updating = shm.him.heading.get() != delayed(0.5, 'shm.him.heading.get()')
#@ sensors.depth.updating = shm.depth.depth.get() != delayed(0.5, 'shm.depth.depth.get()')
#@ sensors.depth.not_crazy = abs(shm.depth.depth.get() - delayed(0.2, 'shm.depth.depth.get()')) < 0.2
#@ thor.sensors.pressure.valid = .7 < shm.pressure.hull.get() < .89
#@ loki.sensors.pressure.valid = .7 < shm.pressure.hull.get() < .89
#@ sensors.pressure.updating = shm.pressure.hull.get() != delayed(0.5, 'shm.pressure.hull.get()')

#@ thor.merge.total_voltage.ok = 26.0 > shm.merge_status.total_voltage.get() > 21.0
#@ thor.merge.current_starboard.ok = 30.0 > shm.merge_status.current_starboard.get() > 2.0
#@ thor.merge.current_port.ok = 30.0 > shm.merge_status.current_port.get() > 2.0
#@ thor.merge.voltage_port.ok = 26.0 > shm.merge_status.voltage_port.get() > 21.0
#@ thor.merge.voltage_starboard.ok = 26.0 > shm.merge_status.voltage_starboard.get() > 21.0

#@ thor.serial.actuator.connected = shm.connected_devices.actuator.get()
#@ thor.serial.gpio.connected = shm.connected_devices.gpio.get()
# #@ thor.serial.him.connected = shm.connected_devices.him.get()
#@ thor.serial.merge.connected = shm.connected_devices.merge.get()
#@ thor.serial.thruster.connected = shm.connected_devices.thruster.get()

# TODO Fix this later
# #@ loki.serial.minipower.connected = shm.connected_devices.minipower.get()
#@ loki.serial.minithruster.connected = shm.connected_devices.minithruster.get()
# #@ loki.serial.powerDistribution.connected = shm.connected_devices.powerDistribution.get()
#@ loki.serial.sensuator.connected = shm.connected_devices.sensuator.get()

# Max % CPU usage = num cores * 100%

#@ thor.sys.cpu_usage.reasonable = float(shell('mpstat | tail -n 1 | sed "s/\s\s*/ /g" | cut -d" " -f4').stdout) < 200.0
#@ loki.sys.cpu_usage.reasonable = float(shell('mpstat | tail -n 1 | sed "s/\s\s*/ /g" | cut -d" " -f4').stdout) < 100.0

# TODO Add back in mem usage test.

# Check for cameras.

# TODO Read these from the configuration?
#@ thor.camera.forward_left.present = shell('test -f /dev/shm/auv_visiond-forward_left').code == 0
#@ thor.camera.forward_right.present = shell('test -f /dev/shm/auv_visiond-forward_right').code == 0
#@ thor.camera.downward.present = shell('test -f /dev/shm/auv_visiond-downward').code == 0
#@ loki.camera.forward.present = shell('test -f /dev/shm/auv_visiond-forward').code == 0
#@ loki.camera.downward.present = shell('test -f /dev/shm/auv_visiond-downward').code == 0

#@ thor.navigation.running = shell('auv-check-navigated').code == 0
#@ thor.hydrophones.pinging = shell('auv-check-pings').code == 0
