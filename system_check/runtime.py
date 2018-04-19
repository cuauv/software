import collections, subprocess, shlex, shm, gevent, imp

procstat = collections.namedtuple('procstat', ['code', 'stdout', 'stderr'])


def proc_running(process):
    return shell('pgrep -f {}'.format(process)).code == 0

def shell(command):
    proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    proc.wait()
    return procstat(proc.returncode, proc.stdout.read(), proc.stderr.read())

def delayed(time, func):
    gevent.sleep(time)
    return func()

def is_changing(func, length = 100):
    val = func()
    updated = False

    iters = 0
    while iters < length:
        if func() != val:
            updated = True
            break
        gevent.sleep(0.01)
        iters += 1

    return updated


