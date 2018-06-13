'''
Approximately Optimal Mission Runner

'''

from functools import *

from mission.opt_aux.aux import *
from mission.framework.task import *
from mission.framework.combinators import *
from mission.framework.movement import *
from mission.framework.primitive import *

from termcolor import colored

import shm, time, numpy, traceback

all_modules = [
  shm.vision_modules.Bins,
  shm.vision_modules.Buoys,
  shm.vision_modules.Debug,
  shm.vision_modules.Wire,
  shm.vision_modules.Pipes,
  shm.vision_modules.Recovery,
  shm.vision_modules.Sonar,
  shm.vision_modules.Stereo,
  shm.vision_modules.Torpedoes
]

def assertModules(modules, log):
  for v in all_modules:
    prev = v.get()
    if modules is not None and  v in modules:
      v.set(True)
      if not prev:
        log('Now enabling vision module: {}'.format(colored(v.__name__, 'green', attrs = ['bold'])))
    else:
      v.set(False)
      if prev:
        log('Now disabling vision module: {}'.format(colored(v.__name__, 'red', attrs = ['bold'])))

def killAllModules(log):
  for v in all_modules:
    prev = v.get()
    v.set(False)
    if prev:
        log('Now disabling vision module: {}'.format(colored(v.__name__, 'red', attrs = ['bold'])))

def prettify(taskPlan, tryCounts):
  return ' => '.join(colored(task.name, 'green', attrs = ['bold']) + ' @ Try ' + colored(tryCounts[task.name], 'cyan', attrs = ['bold']) for task in taskPlan)

def equivalent(planA, planB):
  return len(planA) == len(planB) and all([x.name == y.name for (x, y) in zip(planA, planB)])

class Opt(Task):
  def __init__(self, tasks, restrictions, maxExceptionCount = 5, alreadyFinishedTasks = []):
    super().__init__()
    self.tasks = tasks
    self.restrictions = restrictions
    self.maxExceptionCount = maxExceptionCount
    self.namesToIDs = {task.name: ind for (ind, task) in enumerate(self.tasks)}
    self.idsToNames = {ind: task.name for (ind, task) in enumerate(self.tasks)}
    self.namesToExceptionCounts = {task.name: 0 for task in self.tasks}
    self.taskExecutable = {task.name: True for task in self.tasks}
    self.instances = {task.name: task.cls() for task in self.tasks}
    self.finishedTasks = set(alreadyFinishedTasks)
    self.taskTryCounts = {task.name: 1 for task in self.tasks}

  def on_first_run(self):
    self.initialization = time.time()
    self.lastTask = None
    self.lastMode = None
    self.lastPlan = None
    self.lastTryCount = None

  def on_run(self):
    # Remove any tasks with no progress past timeout
    for (task, instance) in self.instances.items():
      task = self.tasks[self.namesToIDs[task]]
      if not self.taskExecutable[task.name]:
        continue
      if instance.has_run and self.this_run_time - instance.first_run_time > task.noProgressKillTime and not instance.has_made_progress:
        self.logw('Instance of task {} has exceeded no progress kill timeout of {} seconds without making progress!'.format(task.name, task.noProgressKillTime))
        self.taskTryCounts[task.name] += 1
        self.instances[task.name] = self.tasks[self.namesToIDs[task.name]].cls()
        self.logw('Reinstantiated task: {} due to reason: no progress kill timeout.'.format(task.name))
        if self.taskTryCounts[task.name] > task.maxTries:
          self.logw('Task {} has execeeded maximum try count of {}! Will no longer attempt!'.format(task.name, task.maxTries))
          self.taskExecutable[task.name] = False

    # Determine which tasks can be executed
    possibleTasks = set()
    for task in self.tasks:
      if self.taskExecutable[task.name]:
        possibleTasks.add(task.name)

    # Construct topological restrictions
    for restriction in self.restrictions:
      if restriction.beforeTask not in self.finishedTasks:
        possibleTasks.remove(restriction.afterTask)

    plan = []
    for tryCount in range(1, 4):
      for task in self.tasks:
        if task.name in possibleTasks and self.taskTryCounts[task.name] == tryCount:
          plan.append(task)
          possibleTasks.remove(task.name)

    if len(plan) == 0:
      self.logw('No remaining possibly executable tasks! Finishing!')
      self.finish()
    else:
      if self.lastPlan is None or not equivalent(plan, self.lastPlan) or (self.lastTryCount is not None and self.taskTryCounts[task.name] != self.lastTryCount):
        self.logw('New optimal execution plan: {}.'.format(prettify(plan, self.taskTryCounts)))
      self.lastPlan = plan
      task = plan[0]
      if self.lastTask is not None and task.name != self.lastTask.name:
        self.instances[task.name] = self.tasks[self.namesToIDs[task.name]].cls()
        self.logw('Reinstantiated task: {} due to reason: switched.'.format(task.name))
        self.logw('Switched to: task {}!'.format(colored(task.name,'green', attrs = ['bold'])))
      self.lastTask = task
      self.lastTryCount = self.taskTryCounts[task.name]
      assertModules(self.instances[task.name].desiredModules(), self.logi)
      try:
        self.instances[task.name]()
        if self.instances[task.name].finished:
          self.finishedTasks.add(task.name)
          self.taskExecutable[task.name] = False
          self.logw('New finished task set: {}'.format(', '.join(colored(t, 'yellow') for t in self.finishedTasks)))
      except Exception as e:
        self.namesToExceptionCounts[task.name] += 1
        if self.namesToExceptionCounts[task.name] < self.maxExceptionCount:
          self.logw('Task {} threw exception: {}! Exception {} of {} before that task is killed!'.format(task.name, \
            e, self.namesToExceptionCounts[task.name], self.maxExceptionCount))
          traceback.print_exc()
        else:
          self.loge('Task {} threw exception: {}! Task has reached exception threshold, will no longer be attempted!'.format( \
            task.name, e))
          self.taskExecutable[task.name] = False

  def on_finish(self):
    self.loge('Approximately optimal mission complete!')
