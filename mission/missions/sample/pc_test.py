from mission.framework.position import MoveX
from mission.framework.search import SpiralSearch

task = MoveX(0.5)

Spiral = SpiralSearch(optimize_heading = True, min_spin_radius = 2.0)
