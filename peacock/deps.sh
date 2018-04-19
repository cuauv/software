#!/usr/bin/env bash

csc -lzmq -s peacock-internal.scm -j peacock-internal
csc -s queue.scm -j queue
csc -s fishbowl.scm -j fishbowl
