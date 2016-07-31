#!/usr/bin/env python2.7

import json, shm, sys

outfile, data = sys.argv[1], []

try:
    while 1:
        raw_input('Enter to save...')
        real = shm.kalman.heading.get()
        # real = float(raw_input('Enter actual heading: '))
        measured = shm.hydrophones_results.heading.get()
        print('SAVED real:{0} measured:{1}'.format(real, measured))
        data.append((measured, real))

except KeyboardInterrupt:
    print('\nSaving data to \"{0}\"...'.format(outfile))
    open(outfile, 'wb').write(json.dumps(data))
    print('Data saved; terminating.')
