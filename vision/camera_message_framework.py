import struct
import threading
import time

import numpy as np

from collections import namedtuple, OrderedDict
from ctypes import POINTER, c_ubyte, c_uint32, c_uint64, c_char_p, c_voidp, c_ssize_t, c_bool, c_int32, addressof, Structure, create_string_buffer
from functools import reduce
import operator

from auv_python_helpers import load_library
# Return values of _lib.read_frame
FRAMEWORK_QUIT = 1
FRAMEWORK_DELETED = 2

class _Frame(Structure):
    _fields_ = [('data', POINTER(c_ubyte)),
                ('last_frame', c_uint32),
                ('width', c_ssize_t),
                ('height', c_ssize_t),
                ('depth', c_ssize_t),
                ('acq_time', c_uint64)]

_lib_nothread = load_library('libauv-camera-message-framework.so')

_lib_nothread.create_message_framework_from_cstring.argtypes = (c_char_p, c_ssize_t)
_lib_nothread.create_message_framework_from_cstring.restype = c_voidp

_lib_nothread.access_message_framework_from_cstring.argtypes = (c_char_p,)
_lib_nothread.access_message_framework_from_cstring.restype = c_voidp

_lib_nothread.cleanup_message_framework.argtypes = (c_voidp,)
_lib_nothread.cleanup_message_framework.restype = None

_lib_nothread.kill_message_framework.argtypes = (c_voidp,)
_lib_nothread.kill_message_framework.restype = None

_lib_nothread.read_frame.argtypes = (c_voidp, c_voidp)
_lib_nothread.read_frame.restype = c_int32

_lib_nothread.write_frame.argtypes = (c_voidp, c_voidp, c_uint64,
                                      c_ssize_t, c_ssize_t, c_ssize_t)
_lib_nothread.write_frame.restype = c_bool

_lib_nothread.get_buffer_size.argtypes = (c_voidp,)
_lib_nothread.get_buffer_size.restype = c_ssize_t

running = True

# if we're currently running in an eventlet context (i.e. from the GUI)
#   use a proxy for lib. this makes lib calls run in a thread pool, instead
#   of entirely blocking the eventlet cooperative threads
is_patched = 'eventlet' in threading.current_thread.__module__
if is_patched:
    from vision import resizable_eventlet_tpool as tpool
    tpool.setup()
    _lib = tpool.Proxy(_lib_nothread)
else:
    _lib = _lib_nothread

class ExistentialError(Exception):
    pass

class StateError(Exception):
    pass

# Structure that represents an accessor to a shared memory block. Creating an
# accessor will block until the shared memory block with the specified name is
# created
# An accessor has all of the same priviledges as a creator in terms of reading
# and writing frame
class Accessor:
    def __init__(self, name):
        self.name = name
        self._framework = None

        retrying = False

        # loop until the framework actually exists
        while running:
            self._framework = _lib.access_message_framework_from_cstring(name.encode('utf8'))
            if self._framework:
                break

            if not retrying:
                print("Block with name {} does not exist! Waiting and retrying...".format(name))
                retrying = True

            time.sleep(0.5)

        if retrying:
            print("Found {}!".format(name))

        if running:
            self._setup_accessor()

    def valid(self):
        return self._framework

    # convenience method for setting up accessor fields
    # these aren't in the constructor because Creator will also want to use them
    def _setup_accessor(self, framework_valid=True):
        self._frame = _Frame()
        self._array = None
        self._last_frame = None
        if framework_valid:
            self.buffer_size = _lib.get_buffer_size(self._framework)
        self.alive = True

    def get_next_frame(self):
        if not self.alive:
            raise StateError('Accessor has already been cleaned up!')

        frame = self._frame
        ret = _lib.read_frame(addressof(frame), self._framework)
        if ret != 0:
            return ret
            #self.cleanup()
            #raise StateError('Accessor has already been cleaned up!')
        shape = frame.height, frame.width, frame.depth

        # don't recreate the array if we can avoid it, to mitigate numpy memory leak bug
        if (self._array is None or
            self._array.__array_interface__['data'][0] != addressof(frame.data.contents)):
            self._array = np.ctypeslib.as_array(frame.data, shape)
        elif (self._array.shape != shape and
              reduce(operator.mul, self._array.shape) == reduce(operator.mul, shape)):
            self._array = self._array.reshape(shape)
        elif self._array.shape != shape:
            self._array = np.ctypeslib.as_array(frame.data, shape)

        self._last_frame = self._array, frame.acq_time
        return self._last_frame

    def get_last_frame(self):
        if self._last_frame is None:
            raise StateError()
        return self._last_frame

    def has_last_frame(self):
        return self._last_frame is not None

    # write a frame to the shared memory block, with a max dimension size of 3
    # the frame must be at most this.buffer_size bytes
    # acq_time is an int representing the time in miliseconds that the frame
    # was acquired at
    def write_frame(self, frame, acq_time):
        if not self.alive:
            raise StateError('Accessor has already been cleaned up!')

        if len(frame.shape) == 1:
            width, = frame.shape
            depth = height = 1
        elif len(frame.shape) == 2:
            height, width = frame.shape
            depth = 1
        else:
            height, width, depth = frame.shape

        is_live = _lib_nothread.write_frame(self._framework, frame.ctypes.data, acq_time, width, height, depth)
        #if not is_live:
        #    self.cleanup()
        #    raise StateError('Accessor has already been cleaned up!')

    def unblock(self):
        if self._framework is None:
            return
        _lib.kill_message_framework(self._framework)

    def cleanup(self, kill=True):
        if not self.alive:
            return
        self.alive = False
        if self._framework is None:
            return
        _lib.kill_message_framework(self._framework)
        _lib.cleanup_message_framework(self._framework)
        self._framework = None

# A Creator is an accessor that first creates the framework before accessing it.
# It will raise an ExistentialError if the specified name already exists
class Creator(Accessor):
    def __init__(self, name, max_size):
        self.name = name
        self._framework = _lib.create_message_framework_from_cstring(name.encode('utf8'), max_size)
        self._setup_accessor(self._framework)

MAX_NAME_LENGTH = 100
MAX_OPTION_FORMAT_STR_LENGTH = 64
FORMAT_STR_FORMAT_STR = '{}s'.format(MAX_OPTION_FORMAT_STR_LENGTH)

class _OptionAccessor(Accessor):
    def __init__(self, full_name):
        super().__init__(full_name)
        format_str, _ = self.get_next_frame()
        self._setup_option_accessor(format_str)

    def _setup_option_accessor(self, format_str):
        num_zeros = MAX_OPTION_FORMAT_STR_LENGTH - len(format_str)
        # append zeroes to the format string, so that it can be packed
        if num_zeros > 0:
            format_str = bytes(format_str + num_zeros*'\0', 'utf8')
        self._format_str = format_str

    def get_next_frame(self):
        res = super().get_next_frame()
        if isinstance(res, int):
            return res

        frame, _ = res
        self._last_frame = frame
        return self.get_last_frame()

    def get_last_frame(self):
        if not self.has_last_frame():
            raise StateError()
        bstr = self._last_frame.tostring()
        format_bstr = bstr[:MAX_OPTION_FORMAT_STR_LENGTH]
        format_str, = struct.unpack_from(FORMAT_STR_FORMAT_STR, format_bstr)
        value = struct.unpack_from(format_str, bstr[MAX_OPTION_FORMAT_STR_LENGTH:])
        format_str = _char_arr_to_str(format_str).decode('utf8')
        return format_str, value

    def set_value(self, values):
        byteify_tuple = lambda v: bytes(v, 'utf8') if isinstance(v, str) else v
        value_str = struct.pack(self._format_str, *map(byteify_tuple, values))
        narr = np.fromstring(self._format_str + value_str, dtype=np.uint8)
        self.write_frame(narr, int(time.time()*1000))

class _OptionCreator(_OptionAccessor, Creator):
    def __init__(self, full_name, format_str):
        Creator.__init__(self, full_name, MAX_OPTION_FORMAT_STR_LENGTH + struct.calcsize(format_str))
        self._setup_option_accessor(format_str)

MAX_NUM_POSTED_IMAGES = 256
MAX_NUM_OPTIONS = 256
# Gui struct format:
# Number of images (ubyte)
# Number of options (ubyte)
# Location of images (MAX_NAME_LENGTH*MAX_NUM_POSTED_IMAGES bytes)
# Location of options (MAX_NAME_LENGTH*MAX_NUM_OPTIONS bytes)
_gui_struct = struct.Struct('BB{}s{}s'.format(MAX_NAME_LENGTH*MAX_NUM_POSTED_IMAGES,
                                              MAX_NAME_LENGTH*MAX_NUM_OPTIONS))

# convenience method to remove null characters from the end of a string
def _char_arr_to_str(arr):
    try:
        first_null = arr.index(b'\0')
        return arr[:first_null]
    except Exception as e:
        return arr

# convenience method to unpack {num} names from {arr}
def _unpack_names(arr, num):
    lst = []
    pos = 0
    for _ in range(num):
        string = _char_arr_to_str(arr[pos:pos+MAX_NAME_LENGTH])
        lst.append(string)
        pos += MAX_NAME_LENGTH
    return lst

# convenience method to turn a string representing a gui_struct into a tuple
# consisting of (image_names, option_names)
def _gui_struct_to_gui_tuple(s):
    num_images, num_options, im_arr, options_arr = _gui_struct.unpack(s)
    utf8_lambda = lambda s: s.decode('utf8')
    bytes_list_to_str_list = lambda l: list(map(utf8_lambda, l))
    image_names = _unpack_names(im_arr, num_images)
    option_names = _unpack_names(options_arr, num_options)
    return bytes_list_to_str_list(image_names), bytes_list_to_str_list(option_names)

def _construct_gui_shm_name(module_name, value_name, is_option):
    name = '{}_{}'.format(module_name, value_name)
    if is_option:
        name += '_option'
    return name

# Accessor representing a module, containing utility functions for posted
# images, options, etc
class ModuleFrameworkAccessor(Accessor):
    def __init__(self, module_name):
        Accessor.__init__(self, 'module-' + module_name)
        self.cmf_deletion_callback = None
        self._setup_module_framework(module_name)


    def _setup_module_framework(self, module_name):
        self._lock = threading.RLock()
        self._option_accessors = OrderedDict()
        self._image_accessors = {}
        self._image_observers = []
        self._option_observers = []
        self.ordered_image_names = []
        self.ordered_option_names = []
        self._module_name = module_name

        self.threads = []

        t = threading.Thread(target=self._observe_forever)
        t.start()
        self.threads.append(t)

    def _watch_option(self, option_name, option):
        # since an option consumes one of its own frames when it initializes
        #   itself, we need to notify all watchers of the first frame manually
        if option.has_last_frame():
            first_option_value = option.get_last_frame()
            with self._lock:
                for observer in self._option_observers:
                    observer(option_name, first_option_value)

        while running and option_name in self._option_accessors:
            try:
                prev_value = option.get_last_frame()
            except:
                prev_value = None

            next_value = option.get_next_frame()
            if isinstance(next_value, int):
                break

            if prev_value == next_value:
                continue
            if option_name not in self._option_accessors:
                break
            time.sleep(0)
            with self._lock:
                for observer in self._option_observers:
                    observer(option_name, next_value)
            time.sleep(0)

    def _watch_image(self, image_name, image):
        while running and image_name in self._image_accessors:
            res = image.get_next_frame()
            if isinstance(res, int):
                break

            next_value, acq_time = res

            if image_name not in self._image_accessors:
                break
            time.sleep(0)
            with self._lock:
                for observer in self._image_observers:
                    observer(image_name, (next_value, acq_time))
            time.sleep(0)

    def _observe_forever(self):
        while running:
            res = self.get_next_frame()
            if isinstance(res, int):
                if res == FRAMEWORK_DELETED and self.cmf_deletion_callback is not None:
                    self.cmf_deletion_callback()

                break

            frame, _ = res

            bstr = frame.tostring()
            image_names, option_names = _gui_struct_to_gui_tuple(bstr)
            self.ordered_image_names = image_names
            self.ordered_option_names = option_names
            # TODO store image and option indices with Accessor
            def update_watchers(names, accessor_dict, accessor_constructor,
                                is_option):
                # dict_cp tracks the names we have _not_ seen, to be removed
                #   after we add new names
                dict_cp = accessor_dict.copy()
                for name in names:
                    if not name:
                        continue
                    if name not in accessor_dict:
                        accessor_name = _construct_gui_shm_name(self._module_name,
                                                                name, is_option)
                        accessor = accessor_constructor(accessor_name)
                        accessor_dict[name] = accessor

                        if is_option:
                            target=self._watch_option
                        else:
                            target=self._watch_image

                        t = threading.Thread(target=target, args=(name, accessor))
                        t.start()
                    else:
                        del dict_cp[name]
                for name_to_del in dict_cp:
                    # this is plagues by timing issues with creators - e.g. the
                    #   creator will create two options, but this read will only
                    #   pick up the first, deleting the second from the dict.
                    #   This might be fixable with better usage of locks (how?),
                    #   but for now I'm commenting out the deletion, which would
                    #   not give us any notable performance increase anyway
                    #del accessor_dict[name_to_del]
                    pass

            time.sleep(0)
            with self._lock:
                update_watchers(image_names, self._image_accessors, Accessor,
                                False)
                update_watchers(option_names, self._option_accessors,
                                _OptionAccessor, True)
            time.sleep(0)

    def _register_observer(self, observer, observer_list, accessors_dict,
                           notify_with_current_values):
        time.sleep(0)
        with self._lock:
            observer_list.append(observer)
            if notify_with_current_values:
                for name in accessors_dict:
                    accessor = accessors_dict[name]
                    if not accessor.has_last_frame():
                        continue
                    last_accessor_value = accessor.get_last_frame()
                    observer(name, last_accessor_value)

    def register_image_observer(self, image_observer,
                                notify_with_current_values=True):
        self._register_observer(image_observer, self._image_observers,
                                self._image_accessors, notify_with_current_values)

    def register_option_observer(self, option_observer,
                                 notify_with_current_values=True):
        self._register_observer(option_observer, self._option_observers,
                                self._option_accessors, notify_with_current_values)

    def write_image(self, image_name, image, acq_time):
        self._image_accessors[image_name].write_frame(image, acq_time)

    def write_option(self, option_name, value):
        self._option_accessors[option_name].set_value(value)

    def get_option_values(self):
        option_values = {}
        for option_name in self._option_accessors:
            #try:
            option_values[option_name] = self._option_accessors[option_name].get_last_frame()
            #except:
            #    continue
        return option_values

    def get_images(self):
        images = {}
        for image_name, accessor in self._image_accessors.items():
            #try:
            images[image_name] = accessor.get_last_frame()[0]
            #except:
            #    continue
        return images

    def _get_all_CMFs(self):
        return list(self._option_accessors.values()) + \
               list(self._image_accessors.values())

    def unblock(self):
        for CMF in self._get_all_CMFs():
            CMF.unblock()

        super().unblock()

    def register_cmf_deletion_callback(self, f):
        self.cmf_deletion_callback = f

def _safe_key_wrapper(k):
    def get(x):
        try:
            return k(x)
        except:
            return -1
    return get

class ModuleFrameworkCreator(ModuleFrameworkAccessor, Creator):
    def __init__(self, module_name):
        Creator.__init__(self, 'module-' + module_name, _gui_struct.size)
        self._setup_module_framework(module_name)
        self._image_ordering = lambda x: x
        self._option_ordering = lambda x: 0

    def _update(self):
        name_struct_gen = lambda name: '{{:\0<{}}}'.format(MAX_NAME_LENGTH).format(name)

        image_keys = sorted(self._image_accessors.keys(), key=self._image_ordering)
        image_names = map(name_struct_gen, image_keys)
        option_keys = sorted(self._option_accessors.keys(), key=self._option_ordering)
        option_names = map(name_struct_gen, option_keys)

        image_name_str = ''.join(image_names)
        image_name_str += '\0'*(MAX_NAME_LENGTH*MAX_NUM_POSTED_IMAGES - len(image_name_str))
        option_name_str = ''.join(option_names)
        option_name_str += '\0'*(MAX_NAME_LENGTH*MAX_NUM_OPTIONS - len(option_name_str))
        frame = _gui_struct.pack(len(self._image_accessors),
                                 len(self._option_accessors),
                                 image_name_str.encode('utf8'),
                                 option_name_str.encode('utf8'))
        self.write_frame(np.fromstring(frame, dtype=np.uint8), int(time.time() * 1000))

    def create_option(self, option_name, format_str):
        if option_name not in self._option_accessors:
            with self._lock:
                name = _construct_gui_shm_name(self._module_name, option_name, True)
                writer = _OptionCreator(name, format_str)
                self._option_accessors[option_name] = writer
                self._update()
                t = threading.Thread(target=self._watch_option,
                                     args=(option_name, writer))
                t.start()
                self.threads.append(t)

    def create_image(self, image_name, image_buffer_size):
        if image_name not in self._image_accessors:
            with self._lock:
                name = _construct_gui_shm_name(self._module_name, image_name, False)
                writer = Creator(name, image_buffer_size)
                self._image_accessors[image_name] = writer
                self._update()
                t = threading.Thread(target=self._watch_image,
                                     args=(image_name, writer))
                t.start()
                self.threads.append(t)

    def cleanup(self):
        self.unblock()

        for thread in self.threads:
            thread.join()

        for CMF in self._get_all_CMFs():
            CMF.cleanup()

        super().cleanup()

    def set_image_ordering(self, key_function):
        self._image_ordering = _safe_key_wrapper(key_function)
        self._update()

    def set_option_ordering(self, key_function):
        self._option_ordering = _safe_key_wrapper(key_function)
        self._update()
