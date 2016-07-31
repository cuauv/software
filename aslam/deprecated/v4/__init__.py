import sys, os

this = os.path.realpath(__file__)
lib_path = os.path.join(this[:-this[::-1].index('/')], 'lib/python/')
sys.path.append(lib_path)

from aslam_py import *
