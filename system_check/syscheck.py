#!/usr/bin/env python3

import gevent, termcolor, time, argparse, sys, os, signal

CUAUV_VEHICLES = ["thor", "loki"]

#<>#

parser = argparse.ArgumentParser()
parser.add_argument('prefixes', help='Prefixes of tests to run (default: thor)', type=str, nargs='*')
parser.add_argument('-v', '--verbose', help='Display verbose output (default: false)', action='store_true')
parser.add_argument('-c', '--continuous', help='Run in continuous mode (default: false)', action='store_true')
parser.add_argument('-b', '--build', help='Build tests', action='store_true')
parser.add_argument('-d', '--directory', help='Build from specific directory', default=os.environ['CUAUV_SOFTWARE'])
args = parser.parse_args()

if args.build:
    import generate
    generate.construct(args.directory)
    sys.exit(0)

#<>#

def handler(signal, frame):
  sys.stdout.write('Caught Ctrl-C, terminating...')
  sys.exit(0)

signal.signal(signal.SIGINT, handler)

try:
    import gen
except ImportError:
    print("Build tests first with auv-syscheck --build")
    sys.exit(1)

pp = {
    gen.OK: termcolor.colored('OK  ', 'green', attrs = ['bold']),
    gen.WARN: termcolor.colored('WARN', 'yellow', attrs = ['bold']),
    gen.ERR: termcolor.colored('ERR ', 'red', attrs = ['bold'])
}

def run(test, last_result = None):
    result = test.expr()
    if (last_result is None and (args.verbose or not result)) or (last_result is not None and last_result != result):
        p = pp[test.on_success if result else test.on_error]
        sys.stdout.write('[{0}] {1} <| {2} <> FAILED: {3}\n'.format(
            termcolor.colored(time.strftime('%d/%m/%Y %H:%M:%S', time.localtime()), 'cyan'),
            p,
            termcolor.colored(test.name.ljust(40), 'blue'),
            termcolor.colored(test.raw.ljust(50), 'magenta')
        ))
    if args.continuous:
        gevent.sleep(0.5)
        run(test, result)
    else:
        return result

greenlets = []

prefixes_to_run = args.prefixes
default_flag = False
cuauv_vehicle = os.environ.get("CUAUV_VEHICLE")

if prefixes_to_run == []:
  if cuauv_vehicle:
    default_flag = True
    sys.stdout.write(termcolor.colored('Running {0} tests from environment variable CUAUV_VEHICLE along with non-vehicle-specific tests\n'.format(cuauv_vehicle), 'yellow'))
    prefixes_to_run.append(cuauv_vehicle)
  else:
    sys.stdout.write(termcolor.colored('No CUAUV_VEHICLE environment variable or prefix specified\n', 'red'))

# run all tests with the given prefix and any test without a vehicle in the name
for t in gen.tests:
    if any(t.name.startswith(p) for p in prefixes_to_run) or \
            default_flag and (not any(vehicle in t.name for vehicle in CUAUV_VEHICLES)):
        greenlets.append(gevent.spawn(run, t))

gevent.joinall(greenlets)

sys.exit(sum(not x.get() for x in greenlets))
