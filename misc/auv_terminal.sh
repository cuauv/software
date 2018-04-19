#!/usr/bin/env bash

# Runs the specified command (passed on the command line) in a terminal.

cmd=$@

try() {
  if command -v $1 > /dev/null 2>&1; then
    exec $1 $2 ${cmd} 2> /dev/null
    exit 0
  fi
}

try urxvt -e
try gnome-terminal -x
try xterm -e
echo "No suitable terminal found!" \
     "Please add your own in misc/auv_terminal.sh"
exit 1
