from mission.framework.combinators import Sequential
from mission.constants.config import (
  BUOY_BACKUP_DIST,
  BUOY_BACKUP_FOR_OVER_DIST,
  BUOY_OVER_DEPTH,
  BUOY_SEARCH_DEPTH,
  BUOY_TO_PIPE_DIST
)

from mission.framework.movement import Depth
from mission.framework.position import MoveXRough, MoveYRough
from mission.missions.buoys import green, red, scuttle
from mission.missions.pipe import one_pipe
from mission.missions.wire import full as wire_full

# TODO Determine where the yellow buoy is, and avoid it appropriately.
# TODO Move high level buoy logic to buoys.py

buoys = Sequential(
  Depth(BUOY_SEARCH_DEPTH),
  red(),
  MoveXRough(-BUOY_BACKUP_DIST),
  green(),
  MoveXRough(-BUOY_BACKUP_DIST),
  scuttle(),
  MoveXRough(-BUOY_BACKUP_FOR_OVER_DIST),
  Depth(BUOY_OVER_DEPTH),
  MoveYRough(1.2),
  MoveXRough(BUOY_BACKUP_FOR_OVER_DIST + BUOY_TO_PIPE_DIST),
  MoveYRough(-1.2)
)

buoys_no_scuttle = Sequential(
  Depth(BUOY_SEARCH_DEPTH),
  red(),
  MoveXRough(-BUOY_BACKUP_DIST),
  green(),
  MoveXRough(-BUOY_BACKUP_FOR_OVER_DIST),
  Depth(BUOY_OVER_DEPTH),
  MoveXRough(BUOY_BACKUP_FOR_OVER_DIST + BUOY_TO_PIPE_DIST)
)

_teagle = lambda buoys:\
  Sequential(
    one_pipe(),
    buoys,
    one_pipe(),
    wire_full()
  )

teagle = lambda: _teagle(buoys)
teagle_no_scuttle = lambda: _teagle(buoys_no_scuttle)
