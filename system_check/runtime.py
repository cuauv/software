import collections, subprocess, shlex, shm, gevent, imp

tests = []
test = collections.namedtuple('test', ['name', 'expr', 'raw', 'on_success', 'on_error'])
procstat = collections.namedtuple('procstat', ['code', 'stdout', 'stderr'])

OK, WARN, ERR = 0, 1, 2

def proc_running(process):
    return shell('pgrep -f {}'.format(process)).code == 0

def shell(command):
    proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    proc.wait()
    return procstat(proc.returncode, proc.stdout.read(), proc.stderr.read())

def delayed(time, statement):
    gevent.sleep(time)
    return eval(statement)

def define(name, expr, raw, on_success=OK, on_error=ERR):
    tests.append(test(name, expr, raw, on_success, on_error))

def real_import(m):
    mod = __import__(m)
    rem = m.split('.')[1:]
    while len(rem) > 0:
        mod = getattr(mod, rem.pop())
    return mod
