import os
import importlib

importlib.invalidate_caches()
_locale = os.environ['CUAUV_LOCALE']
_config = importlib.import_module('mission.constants.{}'.format(_locale))

# https://stackoverflow.com/questions/21221358/python-how-to-import-all-methods-and-attributes-from-a-module-dynamically
_module_dict = _config.__dict__
try:
    _to_import = _config.__all__
except AttributeError:
    _to_import = [name for name in _module_dict if not name.startswith('_')]
globals().update({name: _module_dict[name] for name in _to_import})
