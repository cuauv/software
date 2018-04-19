"""
This module receives that data from an Android smartphone running the app
"Wireless IMU" and feeds it to shared memory.
"""

import socket
import shm

if __name__ == "__main__":
  sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  sock.bind(("0.0.0.0", 5555))

  write_vec = lambda grp, s, vec: [grp.update(**{"%s_%c" % (s, c) : comp }) \
                                   for comp, c in zip(vec, ("x", "y", "z"))]
  start_inds = { "accel" : 2, "gyro" : 6, "mag" : 10 }

  group = shm.imu
  while 1:
    datas = sock.recv(1024).split(',')
    to_fill = group.get()
    for vec_type, i in start_inds.items():
      if len(datas) > i:
        print datas
        write_vec(to_fill, vec_type, [float(x) for x in datas[i:i+3]])

    # Do an atomic update for each packet. This is important as systems that
    # process each new output may be waiting on a watcher.
    group.set(to_fill)
