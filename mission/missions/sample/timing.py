from mission.framework.timing import Timer, Timeout

# TODO have an actual mission framework test suite?

def TestTimeout(): return Timeout(Timer(2), 1)
