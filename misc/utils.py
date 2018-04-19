import signal
import sys

from threading import Thread, Event

#http://code.activestate.com/recipes/578231-probably-the-fastest-memoization-decorator-in-the-/
def memoize(f):
    """ Memoization decorator for functions taking one or more arguments. """
    class memodict(dict):
        def __init__(self, f):
            self.f = f
        def __call__(self, *args):
            return self[args]
        def __missing__(self, key):
            ret = self[key] = self.f(*key)
            return ret
    return memodict(f)

def killable_function(f):
    thread = Thread(target=f, daemon=True)
    thread.start()
    thread.join()

def register_exit_signals(sigh):
    """
        Given a signal handler sigh that should run on program exit,
        registers all the cuauv-deemed-appropriate signals to that signal
        handler.
    """
    # On Control-C.
    signal.signal(signal.SIGINT, sigh)
    # On pkill.
    signal.signal(signal.SIGTERM, sigh)
    # On closing the commanding terminal.
    signal.signal(signal.SIGHUP, sigh)

def watch_thread_wrapper(f):
    """
        Calls a function f that can be properly shut down on process
        termination. This function will exit the program on SIGINT or SIGTERM.
        f should take in a watcher and an event and should exit when
        the event is set.

        EXAMPLE:
            def f(watcher, quit_event):
                watcher.watch(group_of_interest)

                while not quit_event.is_set():
                    # do things
                    watcher.wait()

            watch_thread_wrapper(f) # Begins the loop above.
    """
    import shm
    watcher = shm.watchers.watcher()
    quit_event = Event()

    thread = Thread(target=f, args=(watcher, quit_event))

    def interrupt_handler(_signal, _frame):
        quit_event.set()
        watcher.disable()
        thread.join()
        sys.exit(0)

    register_exit_signals(interrupt_handler)

    thread.start()
    # XXX: Python HACK, join calls without a timeout do not respond to signals
    while thread.is_alive():
        thread.join(60)
