#!/usr/bin/env bash

pandoc -s --mathjax --toc --smart --number-sections -o aslam/doc/aslam.pdf $1
