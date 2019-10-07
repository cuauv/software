# Full Simulated Mission

import numpy

from mission.opt_aux.aux import *
from mission.missions.opt import Opt
from mission.missions.printer import Printer

class ModedPrinter(Printer):
  def possibleModes(self):
    return [
      Mode(name = 'Mode A', expectedPoints = 100, expectedTime = 10),
      Mode(name = 'Mode B', expectedPoints = 200, expectedTime = 15),
      Mode(name = 'Mode C', expectedPoints = 300, expectedTime = 20)
      ]

printer = lambda n: OptimizableTask(
  name = n,
  instance = ModedPrinter(),
  startPosition = lambda: numpy.array([1, 1, 1])
  )

tasks = [
  printer('Printer 1'),
  printer('Printer 2'), 
  printer('Printer 3'),
  printer('Printer 4')
]

restrictions = [
  TopologicalRestriction(beforeTask = 'Printer 1', afterTask = 'Printer 2'),
  TopologicalRestriction(beforeTask = 'Printer 1', afterTask = 'Printer 3'),
  TopologicalRestriction(beforeTask = 'Printer 1', afterTask = 'Printer 4')
  ]

Sim = lambda: Opt(tasks, restrictions)
