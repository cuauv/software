import os
import json
import time
import collections
import traceback
import base64
import cv2
import tornado.websocket
from tornado.web import HTTPError
from tornado.ioloop import PeriodicCallback

from webserver import BaseHandler
from vision import camera_message_framework, vision_common, options
import shm

module_frameworks = {}
all_vision_modules = vision_common.all_vision_modules()

MAX_IMAGE_DIMENSION = 510
module_listeners = collections.defaultdict(int)

SEND_BUFFER_FLUSH_PERIOD = 50 # Empty send buffer every 50 ms
websocket_listeners = collections.defaultdict(list)
message_send_buffer = {}


def get_active_modules():
    prefix = "auv_visiond-module-"
    return [block[len(prefix):] for block in os.listdir('/dev/shm') if block.startswith(prefix)]


def add_websocket_listener(module_name, websocket_listener):
    websocket_listeners[module_name].append(websocket_listener)
    message_send_buffer[websocket_listener] = []


def remove_websocket_listener(module_name, websocket_listener):
    websocket_listeners[module_name].remove(websocket_listener)
    del message_send_buffer[websocket_listener]


def send_message(websocket_listener, message):
    message_send_buffer[websocket_listener].append(message)


def flush_send_buffer():
    for websocket_listener in message_send_buffer:
        buffer = message_send_buffer[websocket_listener]
        for message in buffer:
            websocket_listener.write_message(message)
        message_send_buffer[websocket_listener] = []


def send_image(module_name, image_name, image, receivers=None):
    if receivers is None:
        receivers = websocket_listeners[module_name]
    try:
        if module_name not in module_frameworks:
            return
        else:
            idx = module_frameworks[module_name].ordered_image_names.index(image_name)

        #print("Sending image {}(data-index={})".format(image_name, idx))
        image = vision_common.resize_keep_ratio(image, MAX_IMAGE_DIMENSION)
        _, jpeg = cv2.imencode('.jpg', image, (cv2.IMWRITE_JPEG_QUALITY, 60))
        jpeg_bytes = base64.encodestring(jpeg.tobytes()).decode('ascii')
        value_dict = {'image_name': image_name,
                      'image': jpeg_bytes,
                      'image_index': idx}
        for websocket_listener in receivers:
            send_message(websocket_listener, value_dict)
    except Exception as e:
        print(e)


def send_option(module_name, option_name, value, receivers=None):
    if receivers is None:
        receivers = websocket_listeners[module_name]
    try:
        if module_name not in module_frameworks:
            return
        else:
            idx = module_frameworks[module_name].ordered_option_names.index(option_name)

        format_str, value = value
        option = options.option_from_value(option_name, format_str, value)
        value_dict = {'option_name': option_name, 'option_index': idx}
        option.populate_value_dict(value_dict)
        #print("Sending option: " + str(value_dict))
        for websocket_listener in receivers:
            send_message(websocket_listener, value_dict)
    except Exception as e:
        print(e)


class VisionSocketHandler(tornado.websocket.WebSocketHandler):
    def __init__(self, *args, **kwargs):
        super(VisionSocketHandler, self).__init__(*args, **kwargs)
        self.module_name = None

    def open(self, module_name):
        print("Received connection to vision gui")
        self.module_name = module_name
        module_listeners[module_name] += 1

        if module_name not in module_frameworks:
            self.close(code=404)

        add_websocket_listener(module_name, self)

        all_option_values = module_frameworks[module_name].get_option_values()
        for option_name, option_value in all_option_values.items():
            send_option(module_name, option_name, option_value, receivers=[self])
        images = module_frameworks[module_name].get_images()
        for image_name, image in images.items():
            send_image(module_name, image_name, image, receivers=[self])

    def on_message(self, message):
        data = json.loads(message)

        # Handle update to module options
        module = data['module'].strip('/')
        if module not in module_frameworks:
            return

        option_name = data['option']
        new_value = data['value']
        #print("Received option update: " + str((option_name, new_value)))
        value_list = list(module_frameworks[module].get_option_values()[option_name][1])
        value_list[0] = new_value
        module_frameworks[module].write_option(option_name, value_list)

    def on_close(self):
        print("Connection to vision gui closed")
        remove_websocket_listener(self.module_name, self)
        module_listeners[self.module_name] -= 1


class VisionIndexHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("vision_index.html"))


class VisionModuleHandler(BaseHandler):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        PeriodicCallback(flush_send_buffer, SEND_BUFFER_FLUSH_PERIOD).start()

    def get_image_observer(self, module_name):
        def observe(name, value):
            if module_listeners[module_name] == 0:
                return

            image, acq_time = value

            if time.time() - acq_time / 1000 > 0.5:
                print('internal lag: {}'.format(time.time() - acq_time / 1000))

            send_image(module_name, name, image)

        return observe

    def get_option_observer(self, module_name):
        def observe(name, value):
            if module_listeners[module_name] == 0:
                return
            send_option(module_name, name, value)

        return observe

    def initialize_module(self, module_name):
        if module_name in module_frameworks:
            # Shutdown the previous framework!
            module_frameworks[module_name].unblock()

        accessor = camera_message_framework.ModuleFrameworkAccessor(module_name)

        # Register the posted images sender.
        image_observer = self.get_image_observer(module_name)
        accessor.register_image_observer(image_observer)

        # Register the options sender.
        option_observer = self.get_option_observer(module_name)
        accessor.register_option_observer(option_observer)

        accessor.register_cmf_deletion_callback(
            lambda module_name=module_name: self.initialize_module(module_name)
        )

        module_frameworks[module_name] = accessor

    def get(self, module_name):
        if module_name not in get_active_modules():
            raise HTTPError(404)

        if hasattr(shm.vision_modules, module_name):
            module_shm = getattr(shm.vision_modules, module_name)
        else:
            module_shm = None

        if module_shm is not None and not module_shm.get():
            raise HTTPError(412)

        try:
            self.initialize_module(module_name)
        except Exception as e:
            print(e)
            traceback.print_exc()
            raise HTTPError(500)
        return self.write(self.render_template('vision_module.html',
                                               template_values={"title": module_name,
                                                                "module_name": module_name}))

    def write_error(self, status_code, **kwargs):
        if status_code == 404:
            self.finish("Could not find the requested module")
        elif status_code == 412:
            self.finish("That module is not currently running"
                        " i.e. It's controlled by a shm variable and that variable is 0")
        else:
            super(VisionModuleHandler, self).write_error(status_code, **kwargs)


class VisionActiveModulesHandler(BaseHandler):
    def get(self):
        return self.write(json.dumps(get_active_modules()))
