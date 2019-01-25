import ctypes
import sys
import threading
import time
import traceback
from collections import OrderedDict

import cv2
import numpy as np

import auv_python_helpers.misc as aph
import shm

from auvlog.client import log as auvlog
from misc.utils import register_exit_signals
from vision import camera_message_framework

from vision.modules.preprocessor import Preprocessor
from vision.framework.helpers import from_umat

logger = auvlog.vision

class UndefinedModuleOption(Exception):
    pass

class CameraDirectionTooLong(Exception):
    pass

class _PsuedoOptionsDict:
    def __init__(self, options_dict):
        self._options_dict = options_dict

    def __getattr__(self, name):
        return self.__getitem__(name)

    def __getitem__(self, name):
        if name not in self._options_dict:
            raise UndefinedModuleOption(name)

        return self._options_dict[name].value

    def __setitem__(self, name, value):
        self._options_dict[name].update(value)

class ModuleBase:
    def __init__(self, default_directions=None, options=None, order_post_by_time=True):
        self.acq_time = -1
        self.order_post_by_time = order_post_by_time
        self.posted_images = []
        self.module_name = self.__class__.__name__
        self.options_dict = OrderedDict()
        self.preprocessor = Preprocessor(self)

        if len(sys.argv) > 1:
            self.directions = sys.argv[1:]
        elif default_directions is None:
            print('No camera directions specified')
            sys.exit(1)
        elif isinstance(default_directions, str):
            self.directions = [default_directions]
        else:
            self.directions = default_directions

        if options is None:
            options = []

        self.running = True

        for option in options:
            self.options_dict[option.name] = option
        self._psuedo_options = _PsuedoOptionsDict(self.options_dict)
        self.module_framework = None

    def fill_single_camera_direction(self, shm_group):
        """
            Used to indicate what direction this (single-camera) module is
            getting images from. Most modules will want to use this.

            This assumes the passed in shm group has a "camera" field
            and uses some shm internals to check its length.
        """
        var_type = [f[1] for f in shm_group._fields_ if f[0] == 'camera'][0]
        max_length = ctypes.sizeof(var_type)
        if len(self.directions[0]) > max_length:
            raise CameraDirectionTooLong("%s (maxlength is %d" % \
                                         (self.directions[0], max_length))

        shm_group.camera = bytes(self.directions[0], encoding='utf-8')

    def post(self, tag, orig_image):
        if type(orig_image) is cv2.UMat:
            image = from_umat(orig_image)
        else:
            image = np.array(orig_image, None, copy=True, order='C', ndmin=1)

        if self.order_post_by_time:
            self.posted_images.append((tag, image))
        else:
            i = 0
            while i < len(self.posted_images) and self.posted_images[i][0] < tag:
                i += 1
            self.posted_images.insert(i, (tag, image))

    def __getattr__(self, item):
        if item in self.__dict__:
            return self.__dict__[item]
        elif item == 'options':
            return self._psuedo_options
        else:
            raise AttributeError()

    def update_CMFs(self):
        self.capture_source_frameworks = [camera_message_framework.Accessor(d) for d in self.directions]
        if self.running:
            self.buffer_size = [f.buffer_size for f in self.capture_source_frameworks]
            self.max_buffer_size = max(self.buffer_size)

        if self.module_framework is not None:
            self.module_framework.cleanup()
        self.make_module_framework()

        self.posted_images_set = set()

    def make_module_framework(self):
        # create the module framework
        suffix = "_on_" + '_'.join(self.directions)
        self.module_framework = camera_message_framework.ModuleFrameworkCreator(self.module_name + suffix)

        #initialize default module values
        for option_name in self.options_dict:
            option = self.options_dict[option_name]
            self.module_framework.create_option(option.name, option.format_str)
            self.module_framework.write_option(option.name, option.get_pack_values())

        self.option_lock = threading.Lock()
        # watcher to reflect option updates in the module framework
        def update_option_watcher(option_name, option_value):
            _, option_value = option_value
            option_value = option_value[0]
            with self.option_lock:
                try:
                    self.options_dict[option_name].update(option_value)
                except ValueError:
                    # will notify self, but shouldn't loop
                    self.module_framework.write_option(option_name, option.get_pack_values())

        self.module_framework.register_option_observer(update_option_watcher, False)

    # Converts coord to normalized coordinates
    # Coord can be either a single number or a tuple
    # If a single number is passed, specify axis
    # axis=0 for x-axis, axis=1 for y-axis
    def normalized(self, coord, axis=None, mat=None):
        if mat is None:
            mat = self._next_images[0]

        def norm(compon, axis):
            return (compon - mat.shape[1-axis]/2)/mat.shape[1]

        if isinstance(coord, tuple):
            return norm(coord[0],0), norm(coord[1],1)
        else:
            return norm(coord, axis)

    # Converts coord to exact coordinates
    # Coord can be either a single number or a tuple
    # If a single number is passed, specify axis
    # axis=0 for x-axis, axis=1 for y-axis
    def denormalized(self, coord, axis=None, mat=None, round=False):
        if mat is None:
            mat = self._next_images[0]

        def denorm(compon, axis):
            denormed = compon * mat.shape[1] + mat.shape[1-axis]/2
            if round:
                denormed = int(denormed)
            return denormed

        if isinstance(coord, tuple):
            return denorm(coord[0],0), denorm(coord[1],1)
        else:
            return denorm(coord, axis)

    def normalized_size(self, size, mat=None):
        if mat is None:
            mat = self._next_images[0]

        def norm(sz): return sz / mat.shape[1] * 2
        try:
            size = iter(size)
            return tuple(norm(sz) for sz in size)
        except TypeError:
            return norm(size)

    def denormalized_size(self, size, mat=None, round=False):
        if mat is None:
            mat = self._next_images[0]

        def denorm(sz):
            denormed = sz * mat.shape[1] / 2
            if round:
                denormed = int(denormed)
            return denormed
        try:
            size = iter(size)
            return tuple(denorm(sz) for sz in size)
        except TypeError:
            return denorm(size)

    # Scales integer option size to the current camera size
    # based on initial value and image width.
    # If isOdd == True, returns an odd integer.
    # If overOne == True, returns an integer greater than one.
    def option_size_int(self, initVal, isOdd=False, overOne=False, mat=None):
        if mat is None:
            mat = self._next_images[0]
        if not isOdd:
            return int(initVal * mat.shape[1])
            if size <= 1 and overOne:
            	return 2
        else:
            size = int(initVal * mat.shape[1] / 2) * 2 + 1
            if size <= 1 and overOne:
                return 3
        return size

    # Scales option size to the current camera size
    # based on initial value and image width.
    def option_size(self, initVal, mat=None):
        if mat is None:
        	mat = self._next_images[0]
        return initVal * mat.shape[1]


    def __call__(self, reload_on_enable=True, reload_on_change=True):
        m_logger = getattr(logger.module, self.module_name)
        m_logger("Running: " + str(self.module_name), True)

        # create the accessor for the capture source framework
        self.update_CMFs()

        quit = threading.Event()
        watcher = shm.watchers.watcher()
        watcher.watch(shm.vision_modules)

        def unblock():
            quit.set()
            watcher.disable()
            [csf.unblock() for csf in self.capture_source_frameworks]

            # This is only to unblock update_CMFs for a second time...
            self.running = False
            camera_message_framework.running = False

        def cleanup():
            main_thread.join(2)

            if main_thread.is_alive():
                m_logger("Failed to kill module thread!", True)
                # How to kill the thread nicely here?

            self.module_framework.cleanup()
            m_logger("All cleaned up.", True)

        def sigh(sig, frame):
            unblock()
            cleanup()
            sys.exit(0)

        def reload_callback():
            self.should_reload = True
            unblock()

        if reload_on_change:
            aph.detect_changes_to_self(reload_callback)

        if hasattr(shm.vision_modules, self.module_name):
            m_logger("Module has shm variable! Now controlled by "
                     "shm.vision_modules.%s" % self.module_name, True)
            module_shm = getattr(shm.vision_modules, self.module_name)
        else:
            module_shm = None

        def func():
            has_been_disabled = False
            while not quit.is_set():
                if module_shm is not None and not module_shm.get():
                    has_been_disabled = True
                    watcher.wait(new_update=False)
                    continue

                if has_been_disabled and reload_on_enable:
                    self.should_reload = True
                    break

                self.posted_images = []

                # Grab images from all capture sources.
                next_images = []
                acq_times = []
                reset = False
                for f in self.capture_source_frameworks:
                    res = f.get_next_frame()
                    if res == camera_message_framework.FRAMEWORK_QUIT:
                        return
                    elif res == camera_message_framework.FRAMEWORK_DELETED:
                        self.update_CMFs()
                        reset = True
                        break

                    next_images.append(res[0])
                    acq_times.append(res[1])

                if reset:
                    continue

                # Feed images to the module.
                with self.option_lock:
                    try:
                        original_options = {option_name: self.options_dict[option_name].value for option_name in self.options_dict}
                        curr_time = time.time()*1000
                        avg_latency = sum(map(lambda t: curr_time - t, acq_times)) / len(acq_times)
                        m_logger.log('{} image latency to {}: {}ms'.format(self.directions, self.module_name, avg_latency))

                        self.acq_time = acq_times
                        self._next_images = self.preprocessor.process(*next_images)
                        self.process(*self._next_images)

                    except Exception as e:
                        sys.stderr.write('{}\n'.format(e))
                        traceback.print_exc(file=sys.stderr)
                        break

                    if quit.is_set():
                        break

                    # if the option value has changed, notify the watchers
                    for option_name in original_options:
                        option = self.options_dict[option_name]
                        original_value = original_options[option_name]
                        if original_value != option.value:
                            self.module_framework.write_option(option.name,
                                                               option.get_pack_values())

                # Deal with any posted images.
                for (tag, image) in self.posted_images:
                    if tag not in self.posted_images_set:
                        self.module_framework.create_image(tag, self.max_buffer_size)
                        img_ordering = {tag: i for (i, (tag, _)) in enumerate(self.posted_images)}
                        self.module_framework.set_image_ordering(lambda x: img_ordering[x])
                        self.posted_images_set.add(tag)
                    self.module_framework.write_image(tag, image, min(acq_times))

        self.should_reload = False
        main_thread = threading.Thread(target=func)

        register_exit_signals(sigh)

        main_thread.start()
        main_thread.join()

        cleanup()

        if self.should_reload:
            aph.reload_self()
