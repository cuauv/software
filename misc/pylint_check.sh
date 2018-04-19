#!/usr/bin/env bash

VER=pylint3

if which pylint3 &> /dev/null; then
  VER=pylint3
elif which pylint &> /dev/null; then
  VER=pylint
else
  echo "Pylint required but not found!"
  exit 1  
fi

$VER --output-format=colorized --reports=n $@
