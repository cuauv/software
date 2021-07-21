# Waypoint

## Motivation
The other freshman and I were testing a buoy-ramming mission the other day, and having a bit of trouble getting it to work. After each failed attempt, we had to drive the sub back into a somewhat specific starting position, and doing so repeatedly was taking some time. Waypoint allows a user to quickly save the sub's position or tell it to return to a saved position through the command, to make repeated tests slightly easier.

## What it does
Waypoint is invoked with "auv-wp" from the command. It has two modes: "--save" and "--restore."
* "--save" or "-s" will save the sub's current position. It optionally takes an argument: the name to be assigned to the sub's current position in memory.
* "--restore" or "-r" will instruct the sub to return to a previously saved position. If given no argument, it returns to the position most-recently saved with no name. If given a name, the sub will return to the corresponding position. Additionally, "--restore" can be given multiple space-separated names, in which case the sub will return to each named position in order.

There are two additional optional parameters: "--dimensions" and "--tolerance."
* "--dimensions" or "-d" can be used with "--restore," and allows the user to instruct the sub to return to a saved position, but only paying attention to certain dimensions. The argument following "--dimensions" should be a string containing lowercase letters which begin the names of the considered dimensions, chosen from [d]epth, [h]eading, [p]itch, [r]oll, [n]orth, [e]ast. For example, "auv-wp -r start -d ned" will tell the sub to return to the position named "start," but not worry about orientation.
* "--tolerance" or "-t" can be used with "--restore" to tell the program how close the sub must be to the desired position before it completes. This is particularly useful when returning to multiple positions in order, because it can be used to decrease the delay between movements.

## How it works
A file called "data.csv" stores all the saved positions. Each line corresponds to one saved position, and has seven comma-separated values: the name of the position and then the six dimensions. The dimensions are ordered depth, heading, pitch, roll, north, east.
The unnamed position (which is used when "--restore" and "--save" are provided no parameter) simply has nothing before its first comma.

## Known Issues
* This isn't exactly a bug, but when using "--tolerance," the provided value is used as a tolerance for both positional dimensions and orientation. (For example, "-t 0.1" tells the sub to get within 0.1 meters in each dimension and 0.1 degrees in each orientation.) This doesn't make particular sense.
* Also not a bug, but this whole tool is useless on Minisub because of it's lack of DVL.
