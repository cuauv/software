#!/usr/bin/env bash

FNAME=$1

ffplay -f lavfi -bufsize 3M -i "movie='pooltest/$FNAME'[01];movie='pooltest-compressed/$FNAME'[02];[02]format=yuva444p,lut=c3=128,negate[video2withAlpha],[01][video2withAlpha]overlay"
