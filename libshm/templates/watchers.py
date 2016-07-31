from shm.base import auv_var_lib

class watcher:
    _create_watcher = auv_var_lib.create_watcher
    _destroy_watcher = auv_var_lib.destroy_watcher
    _wait_watcher = auv_var_lib.wait_watcher
    _has_changed = auv_var_lib.watcher_has_changed
    _broadcast_watcher = auv_var_lib.broadcast_watcher
    _disable_watcher = auv_var_lib.disable_watcher

    
    def __init__(self):
        self.watcher_id = self._create_watcher()
        self.watching=[]

    def wait(self, new_update=True):
        return self._wait_watcher(self.watcher_id, new_update)

    def has_changed(self):
        return self._has_changed(self.watcher_id)

    def watch(self,group):
        group.add_watcher(self)
        self.watching.append(group)

    def unwatch(self,group):
        self.watching.remove(group)
        group.remove_watcher(self)

    def __del__(self):
        self._destroy_watcher(self.watcher_id)

    def broadcast(self):
        self._broadcast_watcher(self.watcher_id)

    def disable(self):
        self._disable_watcher(self.watcher_id)
