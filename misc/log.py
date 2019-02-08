#!/usr/bin/env python3
import logging
import misc.logger_formatting as logger_formatting
import sys
import os

global default_log_level
default_log_level = logging.INFO

#Set up logging
def init_logging(log_level = logging.INFO):
    formatstr = '%(asctime)s.%(msecs)d  %(name)s  %(levelname)s  %(message)s'
    datefmt = '%H:%M:%S'
    global default_logging_level
    default_logging_level = log_level
    console = logging.StreamHandler(sys.stdout)
    console.setLevel(default_logging_level)
    console.setFormatter(logger_formatting.ColoredFormatter(formatstr, datefmt))
    logging.getLogger('').addHandler(console)

#Decorator for linking up a class with a named logger
def with_logging(obj):
    obj.logger = logging.getLogger(obj.__name__)
    obj.log = obj.logger
    obj.log.setLevel(default_log_level)
    return obj

#Decorator supresses stdout of a function call
#TODO: I want to supress opencv's spam... but redirecting stdout doesn't work
def supress_output(fun):
    def wrapper(self, *args):
        saved_stdout, saved_stderr = sys.stdout, sys.stderr
        sys.stdout = sys.stderr = open(os.devnull, "w")
        fun(self, *args)
        sys.stdout, sys.stderr = saved_stdout, saved_stderr
    return wrapper

