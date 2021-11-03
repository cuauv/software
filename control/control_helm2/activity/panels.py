import math
import time
import os

from helm_basis import Panel

mission_filepath = '/home/software/cuauv/workspaces/worktrees/master/control/control_helm2/activity/mission.csv'

# A panel which displays names from the list of connections
class ConnectionsPanel(Panel):
    def __init__(self, title, height, min_recency=0, max_recency=math.inf):
        self.min_recency = min_recency
        self.max_recency = max_recency
        super().__init__(title=title, width=3, height=height)

    def get_cols_lines(self, width, height):
        with open('/home/software/cuauv/workspaces/worktrees/master/control/control_helm2/activity/activity.csv', 'r') as f:
            lines = f.readlines()
        return [[line.split(',')[0] for line in filter(lambda x: time.time() - float(x.split(',')[1]) > self.min_recency and time.time() - float(x.split(',')[1]) <= self.max_recency, lines)]]

# A panel which displays up to one name from the mission file
class MissionPanel(Panel):
    def __init__(self):
        super().__init__(title="Mission", width=7, height=4)

    def get_cols_lines(self, width, height):
        if not os.path.exists(mission_filepath):
            open(mission_filepath, 'w')
        with open(mission_filepath, 'r') as f:
            lines = f.readlines()
        if len(lines) != 0:
            return [[lines[0]]]
        return [[]]

