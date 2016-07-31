from shm_tools.shmlog.parser import LogParser
import numpy as np

def log_to_array(logname, targets):
    parse = LogParser(logname)

    times = []
    values = []

    current_state = [0 for v in targets]

    index = dict( [(v,i) for i,v in enumerate(targets)] )

    while not parse.finished_parsing():
        time, changes = parse.parse_one_slice()

        changed = False
        for var, name, type, value in changes:
            if name in index:
                current_state[index[name]] = float(value)
                changed = True

        if changed:
            times.append(time)
            values.append(current_state[:])

    start = times[0]
    times = np.array([(t-start).microseconds/1e6+(t-start).seconds for t in times])
    values =  np.array(values).T
    return times, values


import sys
if __name__=='__main__':
    times, values = log_to_array(sys.argv[1], ["depth.depth", "kalman.heading"])
    
