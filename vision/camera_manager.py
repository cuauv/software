#!/usr/bin/env python3

from conf.vehicle import cameras
from misc.process_wrapper import ProcessWrapper

def get_camera_command(camera, direction, camera_id, configuration, size, rotate180="", rotate90=""):
  return "auv-{cam}-camera {direction} {camera_id} {config} {rot180} {rot90} " \
         "size={width}x{height}".format(
            cam=camera, direction=direction,
            camera_id=camera_id, config=configuration, rot180=rotate180, rot90=rotate90,
            width=size[0], height=size[1])

cmds = []
for cam, kvs in cameras.items():
  args_dict = { "direction" : cam, "camera_id" : kvs['id'],
                "configuration" : kvs['camera_name'],
                "size": (kvs['width'], kvs['height']) }

  if 'rotate180' in kvs and kvs['rotate180'] in ["true", "True"]:
    args_dict['rotate180'] = "--rotate180"
  if 'rotate90' in kvs and kvs['rotate90'] in ["true", "True"]:
    args_dict['rotate90'] = "--rotate90"

  cmds.append(get_camera_command(kvs['type'], **args_dict))

pw = ProcessWrapper(*cmds)
pw.run()
pw.wait_and_bind_exit()
