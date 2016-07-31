#!/usr/bin/python2
import functools
import threading

import shm

import visiond


# TODO - this is a very good module to have ready, but it is outdated
# This should definitely be updated to work well with the vision system again

def watch_modules(func_on, func_off):
    all_vision_modules = [(x[0], getattr(shm.vision_modules, x[0])) for x in shm.vision_modules._fields]
    module_states = {module_name: False for (module_name, module) in all_vision_modules}
    watcher = shm.watchers.watcher()
    watcher.watch(shm.vision_modules)
    while True:
        for module_name, module in all_vision_modules:
            if module.get():
                if not module_states[module_name]:
                    module_states[module_name] = True
                    func_on(module_name)
            elif not module.get() and module_states[module_name]:
                module_states[module_name] = False
                func_off(module_name)
        if not watcher.wait(new_update=False):
            print('caught exc')
            watcher = shm.watchers.watcher()
            watcher.watch(shm.vision_modules)


if __name__ == '__main__':
    t = threading.Thread(target=watch_modules, args=(lambda x: visiond.start_modules([x], kill_old_modules=False), lambda x: visiond.stop_modules([x])))
    t.daemon=True
    t.start()
    while True:
        pass
