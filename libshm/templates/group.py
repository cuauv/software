import ctypes

import shm.base
auv_var_lib = shm.base.auv_var_lib

_watch_$!g['groupname']!$ = auv_var_lib.shm_watch_$!g['groupname']!$
_watch_$!g['groupname']!$.argtypes = [ctypes.c_int]
_unwatch_$!g['groupname']!$ = auv_var_lib.shm_unwatch_$!g['groupname']!$
_unwatch_$!g['groupname']!$.argtypes = [ctypes.c_int]

_fields = []
<!--(for k in g['varnames'])-->
    <!--(if g['vars'][k]['type'] == 'string')-->
_fields.append(("$!k!$", ctypes.c_char * ($!g['vars'][k]['length']!$ + 1)))
    <!--(else)-->
_fields.append(("$!k!$", ctypes.$!g['vars'][k]['ptype']!$))
    <!--(end)-->
<!--(end)-->

class group(ctypes.Structure):
    _fields_ = _fields

    def update(self, **values):
        for key in values.keys():
            self.__setattr__(key, values[key])

def add_watcher(watcher):
    return _watch_$!g['groupname']!$(watcher.watcher_id)

def remove_watcher(watcher):
    return _unwatch_$!g['groupname']!$(watcher.watcher_id)

def set(g):
    auv_var_lib.shm_set_$!g['groupname']!$(g)

auv_var_lib.shm_get_$!g['groupname']!$.restype = group
def get():
    return auv_var_lib.shm_get_$!g['groupname']!$()


<!--(for k in g['varnames'])-->
    <!--(if g['vars'][k]['type'] == 'string')-->
class $!k!$(shm.base.ShmVar):
    _get_$!g['groupname']!$_$!k!$ = auv_var_lib.shm_get_$!g['groupname']!$_$!k!$
    _get_$!g['groupname']!$_$!k!$.argtypes = [ctypes.c_char_p]
    _set_$!g['groupname']!$_$!k!$ = auv_var_lib.shm_set_$!g['groupname']!$_$!k!$
    _set_$!g['groupname']!$_$!k!$.argtypes = [ctypes.c_char_p]

    @classmethod
    def get(cls):
        tmp = ctypes.create_string_buffer($!g['vars'][k]['length']!$ + 1)
        cls._get_$!g['groupname']!$_$!k!$(tmp)
        v=tmp.value
        #decode bytes to str in python3
        return v if type(v)==str else v.decode()

    @classmethod
    def set(cls,value):
        cls._set_$!g['groupname']!$_$!k!$(value.encode())
        return value

    <!--(else)-->
class $!k!$(shm.base.ShmVar):
    _get = auv_var_lib.shm_get_$!g['groupname']!$_$!k!$
    _get.argtypes = []
    _get.restype = ctypes.$!g['vars'][k]['ptype']!$
    _set = auv_var_lib.shm_set_$!g['groupname']!$_$!k!$
    _set.argtypes = [ctypes.$!g['vars'][k]['ptype']!$]

    @classmethod
    def get(cls):
        return cls._get()

    @classmethod
    def set(cls, value):
        cls._set(ctypes.$!g['vars'][k]['ptype']!$(value))
    <!--(end)-->

<!--(end)-->
