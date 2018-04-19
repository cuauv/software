#!/usr/bin/env bash

cd ../link-stage

echo "Setting Up Local Shared Vars"

./run.sh ./auv-loadvarlayout ../varlayouts/base.xml

#cd ../link-stage

sudo ./run.sh ./auv-killer &

python ../killbutton/killWatcher.py &

echo "You may use the Kill Button (tm) now."
