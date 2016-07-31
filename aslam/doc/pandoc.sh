#!/bin/sh

pandoc -s --mathjax --toc --smart --number-sections -o aslam/doc/aslam.pdf $1
