#!/bin/bash

###
#
# == autolog.py: Script for automatically linking log files ==
# This allows you to re-link videos and log files to symlinks in /loglink/ and simply 
# restart auv-visiond and auv-shmlog-logplayback instead of changing arguments all over 
# the place, speeding things up significantly
# 
# To Set Up:
#  * sudo mkdir /loglink/
#  * sudo chown -R [your system username] /loglink/
#  * Make sure video_test sets /loglink/ as your working directory (default)
#
# Usage:
#  * Browse to directory containing logs you want to use
#  * Run auv-autolog . [tag] where tag is the tag you used in the
#    creation of the log (this script uses grep to match it)
#  * Start (or restart) auv-visiond video_test and 
#    auv-shmlog-loplayback /loglink/mission.log
# 
# Bugs:
#  * Problems with similar tags (i.e. mission1 and mission12
#    using "mission1" would fail here due to multiple grep hits)
#    (still says linked successfully)
#
# -- Jeff Heidel 2012
# -- Developed @ transdec for quick debugging / vision tuning
#
###


#Get arguments
if [ $# -ne 2 ]; then
    echo "Usage: auv-autolog [directory] [tag name]"
    exit 0
fi

DIRECTORY=$1
TAGNAME=$2

#Eval thingz (fully qualified)
FFILE=$DIRECTORY/$(ls $DIRECTORY | grep -e "$TAGNAME" | grep forward)
FFILE="readlink -f $FFILE"
FFILE="`$FFILE`"

DFILE=$DIRECTORY/$(ls $DIRECTORY | grep -e "$TAGNAME" | grep downward)
DFILE="readlink -f $DFILE"
DFILE="`$DFILE`"

LFILE=$DIRECTORY/$(ls $DIRECTORY | grep -e "$TAGNAME" | grep log)
LFILE="readlink -f $LFILE"
LFILE="`$LFILE`"

pushd /loglink/ &>/dev/null

#Unlink any previous
unlink downward.avi &>/dev/null
unlink forward.avi &>/dev/null
unlink mission.log &>/dev/null

#Link them
if [ -f $FFILE ]; then
    ln -s $FFILE forward.avi
fi

if [ -f $DFILE ]; then
    ln -s $DFILE downward.avi
fi

if [ -f $LFILE ]; then
    ln -s $LFILE mission.log
fi

echo "Files linked successfully"

popd &>/dev/null

exit 0

