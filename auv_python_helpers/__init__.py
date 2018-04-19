__author__ = 'zander'
__all__ = ['colors']

def load_library(library):
  import ctypes
  import os.path
  location = os.path.join(os.path.dirname(__file__), os.pardir, 'link-stage')
  return ctypes.cdll.LoadLibrary(os.path.join(location, library))
