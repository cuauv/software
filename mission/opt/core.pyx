from mission.opt.aux import *

def generate(currentPlan, optimizableTasks, topologicalRestrictions):
  # The names of the tasks we're already planning to do  
  currentTaskNames = set(p.taskName for p in currentPlan)

  # Cannot do any of the after tasks if we haven't done the before task first.
  impermissible = set(r.afterTask for r in topologicalRestrictions if r.beforeTask not in currentTaskNames)
  
  # Possible execution plans
  possible = []

  for remaining in optimizableTasks:

    others = [t for t in optimizableTasks if t.name is not remaining.name]

    # Construct a possible execution plan in which we just skip the task
    skip = generate(currentPlan, others, topologicalRestrictions)
    possible += skip

    if remaining.name in impermissible:
      continue

    for mode in remaining.instance.possible_modes():
      taskPlan = TaskPlan(
        taskName = remaining.name,
        mode = mode
        )   

      # Construct a possible execution plan in which we execute this task with this mode
      remainder = generate(currentPlan + [taskPlan], others, topologicalRestrictions)
      possible += remainder

  if len(optimizableTasks) == 0:
    possible = [currentPlan]

  return possible

def stat(distances, executionPlan, capabilities):
  cdef double points, time
  cdef int num_poss
  points, time, num_poss = 0., 0., len(executionPlan)

  capabilities.speed = max(capabilities.speed, 0.01) # TODO remove

  for ind in range(num_poss):
    currTask = executionPlan[ind]
    nextTask = executionPlan[ind + 1] if ind < num_poss - 1 else None
  
    points += currTask.mode.expectedPoints
    time   += currTask.mode.expectedTime

    if nextTask is not None:
      distance = distances[nextTask.taskName][currTask.taskName]
      time += distance / capabilities.speed

  return ExecutionPlanStat(expectedPoints = points, expectedTime = time)
