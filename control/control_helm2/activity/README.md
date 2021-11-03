# Activity Tracking

## Motivation
It seems a common occurrence at pool tests is for one person to begin to drive the sub, only for it to behave unexpectedly because someone else was already controlling it. The Activity Tracking system attempts to eliminate this minor frustration by making clearly apparent in Control Helm anyone else who might be driving the sub.

## What it does
The rightmost column on Control Helm contains 3 panels:
* The "Active" panel displays a list of people who have issued a command to the sub (through Control Helm) within the past 30 seconds.
* The "Idle" panel lists people who have Control Helm open, but who have not issued a command within the past 30 seconds.
* The "Mission" panel displays up to one name of someone who is currently running a mission on the sub.

## How it works
### "Active" and "Idle"
A file called "activity.csv" stores all the information necessary for generating the "Active" and "Idle" panels. The format is simple: Each line contains the name of a team member who has Control Helm open, the time (in seconds since the epoch) that they last interacted with the Helm, and the number of Helms they currently have open, separated by commas.

When a user opens Control Helm, their name is added to the file next to the number zero, as if the last time they interacted was the epoch, which guarantees they show up originally as "Idle." Whenever they press a key which corresponds to a command, the time next to their name in the file is updated. And when they quit the Helm, if the number of helms they have opened is decreased to 0, the line containing their name is removed. Then the "Active" and "Idle" panels simply select names from the list based on their recency.

### Mission
The Mission Runner uses a lock to prevent any two missions from running on the sub simultaneously. It also writes to a file called "mission.csv" to tell control helm when a mission is running. Specifically, it writes the name of the user who runs a mission to the file when the mission begins, and empties the file when the mission ends (and the lock is released). Then Control Helm need only read this file to see if someone is running a mission.
