#!/usr/bin/env bash

cd ${CUAUV_SOFTWARE}mission/constants

if [ -z "$1" ]; then
	echo "No pool name supplied"
	exit 1
fi
if [ -z "$2" ]; then
	echo "No pool region specified"
	exit 1
fi
if [ ! -f "$1.py" ]; then
	echo "Pool constants file '$PWD/$1.py' not found"
	exit 1
fi
if [ ! -f "$2.py" ]; then
	echo "Pool region constants file '$PWD/$2.py' not found"
	exit 1
fi

if [ -d "__pycache__" ]; then
	rm -r __pycache__
fi
if [ -f "config.py" ]; then
	rm config.py
fi
ln -s $1.py config.py

if [ -f "region.py" ]; then
	rm region.py
fi
ln -s $2.py region.py
