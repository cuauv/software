#!/usr/bin/env python3

import eventlet
eventlet.monkey_patch()

from flask import Flask, render_template, abort
from flask_socketio import SocketIO, emit, join_room, leave_room, \
    close_room, rooms, disconnect, send
from vision import camera_message_framework, vision_common, options
import cv2
import sys
import shm
import time
import os
import functools
import collections
import traceback

software_path = os.environ['CUAUV_SOFTWARE']
template_path = os.path.join(software_path, 'vision/gui/templates')
static_path = os.path.join(software_path, 'vision/gui/static')

app = Flask(__name__, template_folder=template_path, static_folder=static_path)

if len(sys.argv) > 1 and sys.argv[1] in ['-d', '--debug']:
    app.debug = True
socketio = SocketIO(app, async_mode='eventlet', binary=True)

module_frameworks = {}
all_vision_modules = vision_common.all_vision_modules()

MAX_IMAGE_DIMENSION = 510
module_listeners = collections.defaultdict(int)

@app.errorhandler(404)
def page_not_found(e):
    return 'Could not find the requested module', 404

@app.errorhandler(412)
def module_not_running(e):
    return 'That module is not currently running' \
            ' i.e. It\'s controlled by a shm variable and that variable is 0', \
            412

def error_if_invalid_module():
    def wrap(func):
        @functools.wraps(func)
        def inner(*args, **kwargs):
            if app.debug:
                print(args, kwargs)
            module_name = kwargs['module_name']

            #if not hasattr(shm.vision_modules, module_name):
            if module_name not in get_active_modules():
                return abort(404)

            if hasattr(shm.vision_modules, module_name):
                module_shm = getattr(shm.vision_modules, module_name)
            else:
                module_shm = None

            if module_shm is not None and not module_shm.get():
                return abort(412)

            try:
                initialize_module(module_name)
            except Exception as e:
                print(e)
                traceback.print_exc()
                return abort(500)
            return func(*args, **kwargs)
        return inner
    return wrap

#def get_active_modules():
#    return [name for (name, m) in all_vision_modules if m.get()]
def get_active_modules():
    prefix = "auv_visiond-module-"
    return [block[len(prefix):] for block in os.listdir('/dev/shm') if block.startswith(prefix)]

@app.route('/')
def index():
    return render_template('index.html', modules=get_active_modules())

def initialize_module(module_name):
    if module_name in module_frameworks:
        # Shutdown the previous framework!
        module_frameworks[module_name].unblock()

    accessor = camera_message_framework.ModuleFrameworkAccessor(module_name)

    # Register the posted images sender.
    image_observer = get_image_observer(module_name)
    accessor.register_image_observer(image_observer)

    # Register the options sender.
    option_observer = get_option_observer(module_name)
    accessor.register_option_observer(option_observer)

    accessor.register_cmf_deletion_callback(
      lambda module_name=module_name: initialize_module(module_name)
    )

    module_frameworks[module_name] = accessor

@socketio.on('register')
def on_join(data):
    if app.debug:
        print('Received register')

    module = data['module'].strip('/')
    module_listeners[module] += 1
    join_room(module)

    while module not in module_frameworks:
        time.sleep(0.1)

    all_option_values = module_frameworks[module].get_option_values()
    for option_name, option_value in all_option_values.items():
        send_option(module, option_name, option_value)
    images = module_frameworks[module].get_images()
    for image_name, image in images.items():
        send_image(module, image_name, image)

@socketio.on('disconnect')
def on_disconnect():
    if app.debug:
        print('Received disconnect')

    for room in rooms():
        if room in module_listeners:
            module_listeners[room] -= 1

@socketio.on('option_update')
def on_option_update(data):
    module = data['module'].strip('/')
    if module not in module_frameworks:
        return abort(404)

    option_name = data['option']
    new_value = data['value']
    if app.debug:
        print("Received option update: " + str((option_name, new_value)))
    value_list = list(module_frameworks[module].get_option_values()[option_name][1])
    value_list[0] = new_value
    module_frameworks[module].write_option(option_name, value_list)

@socketio.on('toggle_module')
def toggle_module(data):
    module = data['module'].strip('/').split("_")[0]
    print(module)

    if app.debug:
        print("Toggling module {}".format(module))

    module_var = shm._eval("vision_modules.{}".format(module))
    module_var.set(not module_var.get())


def send_image(module_name, image_name, image):
    try:
        if module_name not in module_frameworks:
            return
        else:
            idx = module_frameworks[module_name].ordered_image_names.index(image_name)

        if app.debug:
            print("Sending image {}(data-index={})".format(image_name, idx))
        image = vision_common.resize_keep_ratio(image, MAX_IMAGE_DIMENSION)
        _, jpeg = cv2.imencode('.jpg', image, (cv2.IMWRITE_JPEG_QUALITY, 60))
        jpeg_bytes = jpeg.tobytes()
        value_dict = {'image_name': image_name,
                      'image': jpeg_bytes,
                      'image_index': idx}
        socketio.emit('image', value_dict, room=module_name)
    except Exception as e:
        print(e)

def get_image_observer(module_name):
    def observe(name, value):
        if module_listeners[module_name] == 0:
            time.sleep(0)
            return

        image, acq_time = value

        if time.time() - acq_time / 1000 > 0.5:
            print('lag: {}'.format(time.time() - acq_time / 1000))

        send_image(module_name, name, image)

        time.sleep(0)
    return observe

def send_option(module_name, option_name, value):
    try:
        if module_name not in module_frameworks:
            return
        else:
            idx = module_frameworks[module_name].ordered_option_names.index(option_name)

        format_str, value = value
        option = options.option_from_value(option_name, format_str, value)
        value_dict = {'option_name': option_name, 'option_index': idx}
        option.populate_value_dict(value_dict)
        if app.debug:
            print("Sending option: " + str(value_dict))
        socketio.emit('option', value_dict, room=module_name)
    except Exception as e:
        print(e)

def get_option_observer(module_name):
    def observe(name, value):
        if module_listeners[module_name] == 0:
            time.sleep(0)
            return
        send_option(module_name, name, value)
        time.sleep(0)
    return observe

@app.route('/<module_name>/')
@error_if_invalid_module()
def module(module_name):
    if module_name not in module_frameworks:
        return abort(404)

    images = module_frameworks[module_name].ordered_image_names
    return render_template('module.html', module_name=module_name, modules=get_active_modules())

if __name__ == '__main__':
    print('WARNING: auv-vision-gui is DEPRECATED. Please use auv-webserver instead.')
    # Turn off the default Python ^C handler that raises a KeyboardInterrupt.
    # Now SIGINT should behave identically to SIGTERM: kill the program.
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    socketio.run(app, host='0.0.0.0', port=5000, use_reloader=False)
