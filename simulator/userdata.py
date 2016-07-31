
import os
import os.path
import plistlib

HOME_DIR = os.getenv('HOME')
USR_DATA_DIR = os.path.join(HOME_DIR, '.cuauvsim')
PREFS_PATH = os.path.join(USR_DATA_DIR, 'preferences.plist')

if not os.path.isdir(USR_DATA_DIR):
    os.mkdir(USR_DATA_DIR)

def InitAppPreferences(cls):
    if not os.path.isfile(PREFS_PATH):
        cls.prefs = {}
    else:
        cls.prefs = plistlib.readPlist(PREFS_PATH)
    return cls

@InitAppPreferences
class AppPreferences(object):
    
    @classmethod
    def set(cls, key, value):
        cls.prefs[key] = value
        cls.write()

    @classmethod
    def get(cls, key, default=None):
        return cls.prefs.get(key, default)

    @classmethod
    def write(cls):
        plistlib.writePlist(cls.prefs, PREFS_PATH)
