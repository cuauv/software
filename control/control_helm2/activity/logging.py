import os
import sys
import time

activity_filepath = '/home/software/cuauv/workspaces/worktrees/master/control/control_helm2/activity/activity.csv'

# Read the activity file, returning the raw list of lines as well
# as the number of the line which holds the current user's name
# (-1 if the current user's name does not appear).
def read_file():
    if not os.path.exists(activity_filepath):
        open(activity_filepath, 'w')
    with open(activity_filepath, 'r') as f:
        lines = f.readlines()
    line_num = -1
    for num, line in enumerate(lines):
        if line.split(',')[0] == os.getenv('AUV_ENV_ALIAS'):
            line_num = num
    return (lines, line_num)

# Add the user to the list of connections stored
# in connections.csv: their name followed by
# a timestamp representing when they were last
# active in the control helm.
def log(new_helm=False):
    lines, line_num = read_file()
    if line_num == -1:
        lines.append('')
        helms = 1
    else:
        helms = int(lines[line_num].split(',')[2]) + (1 if new_helm else 0)
    lines[line_num] = os.getenv('AUV_ENV_ALIAS') + ',' + str(time.time()) + ',' + str(helms) + '\n'
    with open(activity_filepath, 'w') as f:
        f.writelines(lines)

# Add a call to 'log()' to each key callback
# so the last time the user is marked as
# having been active always stays up to date.
def add_logging(callbacks, key):
    value = callbacks[key]
    def func():
        log()
        return value()
    callbacks[key] = func

# Remove the user from the activity list if closing
# their last Helm, otherwise simply decrease their
# helm count by 1.
def close_helm(signum, frame):
    lines, line_num = read_file()
    helms = int(lines[line_num].split(',')[2])
    if helms == 1:
        lines.remove(lines[line_num])
    else:
        time = float(lines[line_num].split(',')[1])
        lines[line_num] = os.getenv('AUV_ENV_ALIAS') + ',' + str(time) + ',' + str(helms - 1) + '\n'
    with open(activity_filepath, 'w') as f:
        f.writelines(lines)
    sys.exit(0)

