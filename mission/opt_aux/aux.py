'''

Optimal Mission Runner Types / Auxiliary

Separated out for ease-of-import.

'''

from collections import namedtuple
from enum import Enum

'''
OptimizableTask: A task that can be run by the optimal mission runner.

OptimizableTasks must specify:
  name :: string
  cls :: class (must be subtask of Task)
  startPosition :: () -> numpy 3-vector (north, east, depth)
'''

OptimizableTask = namedtuple('OptimizableTask', [
  'name',
  'cls',
  'maxTries',
  'noProgressKillTime'
  ])

'''
Topological Restriction: A required ordering of tasks.

TopologicalRestrictions must specify
  beforeTask :: string
  afterTask  :: string
'''

TopologicalRestriction = namedtuple('TopologicalRestriction', [
  'beforeTask',
  'afterTask'
  ])
