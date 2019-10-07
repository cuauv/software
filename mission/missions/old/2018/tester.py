
from mission.framework.search import HeadingSearch
import shm

test = HeadingSearch(initial_heading=shm.kalman.heading.get())
