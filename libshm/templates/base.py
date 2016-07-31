import ctypes
from abc import ABCMeta, abstractmethod

from auv_python_helpers import load_library
auv_var_lib = load_library("libshm.so")

class ShmVar(object):
    __metaclass__ = ABCMeta
    """ Base class for python shared memory variables """

    @classmethod
    @abstractmethod
    def get(cls):
        pass

    @classmethod
    @abstractmethod
    def set(cls, value):
        pass
